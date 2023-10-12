{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Resolution.Utils (scheme, parallelized, reverseDependencies, ParallelizedState (..)) where

import Control.Concurrent.Async.Lifted (Async, async, wait, forConcurrently)
import Control.Concurrent.MVar.Lifted
import Control.Lens (makeLenses, use, (%=), (.=))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadState (get))
import Control.Monad (when)
import Control.Monad.State (MonadIO (..), MonadTrans (..), StateT, evalStateT, execStateT, gets)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Data.Set as Set
import GTD.Utils (flipTuple, logDebugNSS, mapFrom, updateStatus)
import Text.Printf (printf)

data SchemeState k a b = SchemeState
  { _schemeStateA :: Map.Map k a,
    _schemeStateB :: Map.Map k b
  }

$(makeLenses ''SchemeState)

_scheme ::
  (Ord k, MonadBaseControl IO m) =>
  (b -> m (Maybe a)) ->
  (b -> k) ->
  (a -> m [b]) ->
  [b] ->
  StateT (SchemeState k a b) m ()
_scheme f kb p ks = do
  z <- lift $ catMaybes <$> forConcurrently ks (\x -> fmap (x,) <$> f x)
  let (bs1, as1) = unzip z
  let ks1 = kb <$> bs1
  schemeStateA %= Map.union (Map.fromList $ zip ks1 as1)
  schemeStateB %= Map.union (Map.fromList $ zip ks1 bs1)
  deps <- lift $ concat <$> forConcurrently as1 p
  let ds = mapFrom kb deps
  _ks <- use schemeStateB
  let ds' = ds `Map.difference` _ks
  if Map.null ds'
    then return ()
    else _scheme f kb p (Map.elems ds')

graph ::
  (Ord k, MonadBaseControl IO m) =>
  (b -> m (Maybe a)) ->
  (a -> k) ->
  (b -> k) ->
  (a -> m [b]) ->
  [b] ->
  StateT (SchemeState k a b) m (Graph.Graph, Map.Map k Graph.Vertex, Map.Map Graph.Vertex a)
graph f ka kb p ks = do
  _scheme f kb p ks
  as <- gets _schemeStateA

  let iModules = Map.fromList $ zip [1 ..] (Map.elems as)
  let iNModules = Map.fromList $ flipTuple <$> Map.assocs (ka <$> iModules)

  let jF i dep = (i, fromJust $ Map.lookup dep iNModules)
      iF i ds = jF i <$> filter (`Map.member` iNModules) ds
  edges <- lift $ concat <$> mapM (\(i, m) -> iF i <$> ((kb <$>) <$> p m)) (Map.assocs iModules)
  return $ (,,) (Graph.buildG (1, Map.size as) edges) iNModules iModules

-- | `scheme` recursively figures out dependencies for a given root set
-- it is safe in terms of cyclic dependencies - `a` is called at most once for a given key
-- returns list of values ordered by dependencies (topological sort; if item A depends on item B, then B comes before A)
-- types:
-- `a` is a value
-- `b` is a value that is used to produce `a`; `b` is not used for deduplication
-- `k` is a key for both `a` and `b` to be used for `deduplication`
-- `b` and `k` can be the same type, in case `k` is enough to generate `a`
-- arguments:
-- `f` is a builder argument, that is called at most once to build a value for a key
-- `ka`, `kb` - key producers for `a` or `b` respectively
-- `p` is a function that produces dependencies for a value
-- `ks` is a set of keys to start with (root set)
schemeS ::
  (Ord k, MonadBaseControl IO m) =>
  (b -> m (Maybe a)) ->
  (a -> k) ->
  (b -> k) ->
  (a -> m [b]) ->
  [b] ->
  StateT (SchemeState k a b) m [a]
schemeS f ka kb p ks = do
  (g, _, vToA) <- graph f ka kb p ks
  let cs = concatMap (foldr (:) []) $ Graph.scc g
  return $ fromJust . (`Map.lookup` vToA) <$> cs

reverseDependenciesS ::
  (Ord k, MonadBaseControl IO m) =>
  (b -> m (Maybe a)) ->
  (a -> k) ->
  (b -> k) ->
  (a -> m [b]) ->
  [b] ->
  k ->
  StateT (SchemeState k a b) m (Maybe [a])
reverseDependenciesS f ka kb p ks k = do
  (g, kToV, vToA) <- graph f ka kb p ks
  let gT = Graph.transposeG g
  let ds = mapMaybe (`Map.lookup` vToA) <$> (Graph.reachable gT <$> Map.lookup k kToV)
  return ds

reverseDependencies f ka kb p ks k = evalStateT (reverseDependenciesS f ka kb p ks k) (SchemeState Map.empty Map.empty)

scheme f ka kb p ks = evalStateT (schemeS f ka kb p ks) (SchemeState Map.empty Map.empty)

---

-- `_queue` is a queue of tasks to be executed
-- `_processed` is a map of tasks that have been executed
-- `_asyncs` is a map of tasks that are currently executing
-- `_state` is a state that is passed to `f` and `ps`
-- `_shouldUpdateStatus` is a flag that indicates whether status file should be updated
data ParallelizedState s k b m = ParallelizedState
  { _queue :: [b],
    _processed :: Set.Set k,
    _asyncs :: Map.Map k (Async (StM m (s -> s))),
    _state :: s,
    _shouldUpdateStatus :: Bool
  }

$(makeLenses ''ParallelizedState)

-- | `parallelized` executes a given function in parallel, while respecting dependencies
-- types and arguments format is the same as for `scheme`, plus:
-- type `n` is used as an additional debug logging context
-- `f` is a function that produces a value; it accepts current state as an argument and generates a state update function
--     as `f` is executed concurrently, the state update function is only applied in the 'main' thread
--     the idea was to avoid having MVar's, but it's not clear whether this was a good idea
-- `ps` is a function that produces a string representation of the state for debug logging
-- `ds` is a function that produces dependencies for a value; given that we don't need to figure out the execution order,
--      this function is used to figure out whether a given task (`b`) depends on a task that is currently executing;
--      in such a case, we have to await on the required tasks
parallelized ::
  (Show b, Show n, Show k, Ord k) =>
  (MonadIO m, MonadLoggerIO m, MonadBaseControl IO m) =>
  ParallelizedState s k b m ->
  n ->
  (s -> b -> m (s -> s)) ->
  (s -> String) ->
  (b -> k) ->
  (b -> m [k]) ->
  m s
parallelized s n f ps kb ds = _state <$> execStateT (parallelized0 n f ps kb ds) s

parallelized0 ::
  (Show b, Show n, Show k, Ord k) =>
  (MonadIO m, MonadLoggerIO m, MonadBaseControl IO m) =>
  n ->
  (s -> b -> m (s -> s)) ->
  (s -> String) ->
  (b -> k) ->
  (b -> m [k]) ->
  StateT (ParallelizedState s k b m) m ()
parallelized0 n f ps kb ds = do
  ParallelizedState {_queue = q, _asyncs = a, _shouldUpdateStatus = us} <- get
  let logTag = printf "parallelized %s" (show n)
  finishedButNotConsumed <- newMVar (0 :: Int)

  case q of
    [] -> do
      let aK = Map.keysSet a
      ss <- lift $ mapM wait (Map.elems a)
      state %= flip (foldr ($)) ss
      processed %= Set.union aK
    (x : xs) -> do
      xD <- lift $ ds x
      let is = Set.intersection (Set.fromList xD) (Map.keysSet a)
      if Set.null is
        then do
          logDebugNSS logTag (printf "asyncs=%s, queue=%s" (show $ length a) (show $ length xs))
          s <- use state
          a <- lift $ async $ do
            logDebugNSS logTag $ printf "%s starting;\ndeps = %s\nstate = %s" (show $ kb x) (show xD) (ps s)
            z <- f s x
            modifyMVar_ finishedButNotConsumed $ return . (+ 1)
            logDebugNSS logTag $ printf "%s finished" (show $ kb x)
            return z
          asyncs %= Map.insert (kb x) a
          queue .= xs
        else do
          let (aK, xDA) = unzip $ mapMaybe (\y -> (y,) <$> (y `Map.lookup` a)) (Set.toList is)
          ss <- lift $ mapM wait xDA
          modifyMVar_ finishedButNotConsumed $ return . (`subtract` length xDA)

          processed %= Set.union (Set.fromList aK)
          asyncs %= flip (foldr Map.delete) is

          s0 <- use state
          state .= foldr ($) s0 ss
          s1 <- use state

          fbnc <- readMVar finishedButNotConsumed
          when us $
            updateStatus $
              printf "%s: %s tasks executing, %s in queue" logTag (show $ length a - fbnc) (show $ length xs)
          logDebugNSS logTag $
            printf
              "asyncs=%s, queue=%s\nwaited for=%s\nss=%s\ns0=%s\ns1=%s\nus=%s"
              (show $ length a)
              (show $ length xs)
              (show xD)
              (show $ length ss)
              (ps s0)
              (ps s1)
              (show us)

      parallelized0 n f ps kb ds

  return ()
