{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Resolution.Utils where

import Control.Lens (makeLenses, (%=), use)
import qualified Data.Map.Strict as Map
import Control.Monad.State (StateT, gets, MonadTrans (..))
import qualified Data.Graph as Graph
import GTD.Utils (flipTuple, mapFrom)
import Data.Maybe (fromJust, catMaybes)

data SchemeState k a b = SchemeState
  { _schemeStateA :: Map.Map k a,
    _schemeStateB :: Map.Map k b
  }

$(makeLenses ''SchemeState)

-- | `a` is a builder argument, that is called at most once to build a value for a key
-- | `k` is a key produced for a value
-- | `p` is a function that produces dependencies for a value
-- | `ks` is a set of keys to start with (root set)
-- | `scheme` recursively figures out dependencies for a given root set;
-- |          it is safe in terms of cyclic dependencies - `a` is called at most once for a given key
_scheme ::
  (Ord k, Monad m) =>
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
  (Ord k, Monad m) =>
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
  let graphS = Graph.reverseTopSort graph

  return $ fromJust . (`Map.lookup` iModules) <$> graphS
