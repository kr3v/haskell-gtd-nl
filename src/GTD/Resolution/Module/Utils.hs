{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- those are utility functions for using the 'resolution' map
module GTD.Resolution.Module.Utils where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Identifier, SourceSpan (..), asDeclsHMap, emptySourceSpan)
import qualified GTD.Haskell.Parser.GhcLibParser as GHC
import GTD.Utils (logDebugNSS, maybeToMaybeT)
import Text.Printf (printf)

resolution'simplify :: Declarations -> HMap.HashMap Identifier Declaration
resolution'simplify Declarations {_decls = ds, _dataTypes = dts} =
  let ds' = Map.elems ds
      dts' = concatMap (\cd -> [_cdtName cd] <> Map.elems (_cdtFields cd)) (Map.elems dts)
   in asDeclsHMap $ ds' <> dts'

resolution'qualified ::
  HMap.HashMap String Declarations ->
  String ->
  (MonadLoggerIO m, MonadError String m) => MaybeT m SourceSpan
resolution'qualified rm w = do
  y <- maybeToMaybeT $ w `HMap.lookup` rm
  case HMap.elems $ resolution'simplify y of
    (d : _) -> return $ emptySourceSpan {sourceSpanFileName = sourceSpanFileName . _declSrcOrig $ d, sourceSpanStartColumn = 1, sourceSpanStartLine = 1}
    _ -> throwError "given word is a known module, but it has no declarations"

resolution'word ::
  HMap.HashMap String Declarations ->
  String ->
  (MonadLoggerIO m, MonadError String m) => MaybeT m SourceSpan
resolution'word rm w = do
  let (_, (q, w')) = fromMaybe (emptySourceSpan, ("", w)) $ GHC.identifier w
  let look = resolution'word'parsed rm
  let cases = if w == w' then [("", w)] else [(q, w'), ("", w)]
  casesM <- forM cases $ \(q1, w1) -> do
    let r = look q1 w1
    logDebugNSS "definition" $ printf "%s -> `%s`.`%s` -> %s" w q1 w1 (show r)
    return r
  case catMaybes casesM of
    (x : _) -> return x
    _ -> MaybeT $ return Nothing

resolution'word'parsed ::
  HMap.HashMap String Declarations ->
  String ->
  String ->
  Maybe SourceSpan
resolution'word'parsed rm q1 w1 = do
  mQ <- HMap.lookup q1 rm
  d <- HMap.lookup w1 $ resolution'simplify mQ
  Just $ _declSrcOrig d

resolution'word'parsed'b ::
  HMap.HashMap String (HMap.HashMap Identifier Declaration) ->
  String ->
  String ->
  Maybe SourceSpan
resolution'word'parsed'b rm q1 w1 = do
  mQ <- HMap.lookup q1 rm
  d <- HMap.lookup w1 mQ
  Just $ _declSrcOrig d