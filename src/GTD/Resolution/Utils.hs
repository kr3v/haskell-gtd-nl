{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Resolution.Utils where

import Control.Concurrent.Async.Lifted (Async, async, wait)
import Control.Lens (makeLenses, use, (%=), (.=))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (mapAndUnzipM)
import Control.Monad.State (MonadIO (..), MonadState (get), MonadTrans (..), StateT, evalStateT, execStateT, gets)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Data.Set as Set
import GTD.Utils (flipTuple, logDebugNSS, mapFrom)
import Text.Printf

data SchemeState k a b = SchemeState
  { _schemeStateA :: Map.Map k a,
    _schemeStateB :: Map.Map k b
  }

$(makeLenses ''SchemeState)

-- | types: `a` is a value; `b` is a key for a value, yet it cannot be used for deduplication; `k` is a key for a value, that can be used for deduplication; `k` \in `b`, `b` \in `a`
-- | arguments:
-- | `a` is a builder argument, that is called at most once to build a value for a key
-- | `k` is a key produced for a value
-- | `p` is a function that produces dependencies for a value
-- | `ks` is a set of keys to start with (root set)
-- |
-- | `scheme` recursively figures out dependencies for a given root set;
-- |          it is safe in terms of cyclic dependencies - `a` is called at most once for a given key
_scheme ::
  (Ord k, MonadLoggerIO m) =>
  (b -> m (Maybe a)) ->
  (b -> k) ->
  (a -> m [b]) ->
  [b] ->
  StateT (SchemeState k a b) m ()
_scheme a k p ks = do
  z <- lift $ catMaybes <$> mapM (\x -> fmap (x,) <$> a x) ks
  let (bs1, as1) = unzip z
  let ks1 = k <$> bs1
  schemeStateA %= Map.union (Map.fromList $ zip ks1 as1)
  schemeStateB %= Map.union (Map.fromList $ zip ks1 bs1)
  deps <- lift $ concat <$> mapM p as1
  let ds = mapFrom k deps
  _ks <- use schemeStateB
  let ds' = ds `Map.difference` _ks
  if Map.null ds'
    then return ()
    else _scheme a k p (Map.elems ds')

scheme ::
  (Ord k, MonadLoggerIO m) =>
  (b -> m (Maybe a)) ->
  (a -> k) ->
  (b -> k) ->
  (a -> m [b]) ->
  [b] ->
  StateT (SchemeState k a b) m [a]
scheme a ka kb p ks = do
  _scheme a kb p ks
  as <- gets _schemeStateA

  let iModules = Map.fromList $ zip [1 ..] (Map.elems as)
  let iNModules = Map.fromList $ flipTuple <$> Map.assocs (ka <$> iModules)

  let jF i dep = (i, fromJust $ Map.lookup dep iNModules)
      iF i ds = jF i <$> filter (`Map.member` iNModules) ds
  edges <- lift $ concat <$> mapM (\(i, m) -> iF i <$> ((kb <$>) <$> p m)) (Map.assocs iModules)
  let graph = Graph.buildG (1, Map.size as) edges
  let graphS = concatMap (foldr (:) []) (Graph.scc graph)

  return $ fromJust . (`Map.lookup` iModules) <$> graphS

data ParallelizedState s k a b m = ParallelizedState
  { _queue :: [a],
    _processed :: Map.Map k b,
    _asyncs :: Map.Map k (Async (StM m (b, s -> s))),
    _state :: s
  }

$(makeLenses ''ParallelizedState)

parallelized ::
  (Show a, Show b, Show n, Show k, MonadIO m, MonadLoggerIO m, MonadBaseControl IO m, Ord k) =>
  ParallelizedState s k a b m ->
  n ->
  (s -> a -> m (b, s -> s)) ->
  (s -> String) ->
  (a -> k) ->
  (a -> m [k]) ->
  m s
parallelized s n f ps ka ds = _state <$> execStateT (parallelized0 n f ps ka ds) s

parallelized0 ::
  (Show a, Show b, Show n, Show k, MonadIO m, MonadLoggerIO m, MonadBaseControl IO m, Ord k) =>
  n ->
  (s -> a -> m (b, s -> s)) ->
  (s -> String) ->
  (a -> k) ->
  (a -> m [k]) ->
  StateT (ParallelizedState s k a b m) m ()
parallelized0 n f ps ka ds = do
  q <- use queue
  a <- use asyncs

  case q of
    [] -> do
      let aK = Map.keys a
      (rs, ss) <- lift $ mapAndUnzipM wait (Map.elems a)
      state %= flip (foldr ($)) ss
      processed %= Map.union (Map.fromList $ zip aK rs)
    (x : xs) -> do
      xD <- lift $ ds x

      let is = Set.intersection (Set.fromList xD) (Map.keysSet a)
      if Set.null is
        then do
          logDebugNSS ("parallelized " ++ show n) (printf "asyncs=%s, queue=%s" (show $ length a) (show $ length xs))
          s <- use state
          a <- lift $ async $ do
            logDebugNSS (printf "parallelized %s %s" (show n) (show $ ka x)) $ printf "starting;\ndeps = %s\nstate = %s" (show xD) (ps s)
            z <- f s x
            logDebugNSS (printf "parallelized %s %s" (show n) (show $ ka x)) "finished"
            return z
          asyncs %= Map.insert (ka x) a
          queue .= xs
        else do
          let (aK, xDA) = unzip $ mapMaybe (\y -> (y,) <$> (y `Map.lookup` a)) (Set.toList is)
          (rs, ss) <- lift $ mapAndUnzipM wait xDA

          processed %= Map.union (Map.fromList $ zip aK rs)
          asyncs %= flip (foldr Map.delete) is

          s0 <- use state
          state .= foldr ($) s0 ss
          s1 <- use state

          logDebugNSS (printf "parallelized %s" (show n)) $
            printf
              "asyncs=%s, queue=%s\nwaited for=%s\nss=%s\ns0=%s\ns1=%s"
              (show $ length a)
              (show $ length xs)
              (show xD)
              (show $ length ss)
              (ps s0)
              (ps s1)

      parallelized0 n f ps ka ds

  return ()