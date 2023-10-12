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
import Data.Maybe (catMaybes, fromMaybe)
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Identifier, SourceSpan (..), asDeclsHMap, emptySourceSpan)
import qualified GTD.Haskell.Parser.GhcLibParser as GHC
import GTD.Utils (logDebugNSS, maybeToMaybeT)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as BSC8

resolution'simplify :: Declarations -> HMap.HashMap Identifier Declaration
resolution'simplify Declarations {_decls = ds, _dataTypes = dts} =
  let ds' = HMap.elems ds
      dts' = concatMap (\cd -> [_cdtName cd] <> HMap.elems (_cdtFields cd)) (HMap.elems dts)
   in asDeclsHMap $ ds' <> dts'

resolution'qualified ::
  HMap.HashMap Identifier Declarations ->
  BSC8.ByteString ->
  (MonadLoggerIO m, MonadError String m) => MaybeT m SourceSpan
resolution'qualified rm w = do
  y <- maybeToMaybeT $ w `HMap.lookup` rm
  case HMap.elems $ resolution'simplify y of
    (d : _) -> return $ emptySourceSpan {_fileName = _fileName . _declSrcOrig $ d, _colBegin = 1, _lineBegin = 1, _colEnd = 1, _lineEnd = 1}
    _ -> throwError "given word is a known module, but it has no declarations"

resolution'word ::
  HMap.HashMap Identifier Declarations ->
  BSC8.ByteString ->
  (MonadLoggerIO m, MonadError String m) => MaybeT m SourceSpan
resolution'word rm w = do
  let (_, (q, w')) = fromMaybe (emptySourceSpan, (mempty, w)) $ GHC.identifier w
  let look = resolution'word'parsed rm
  let cases = if w == w' then [(mempty, w)] else [(q, w'), (mempty, w)]
  casesM <- forM cases $ \(q1, w1) -> do
    let r = look q1 w1
    logDebugNSS "definition" $ printf "%s -> `%s`.`%s` -> %s" (show w) (show q1) (show w1) (show r)
    return r
  case catMaybes casesM of
    (x : _) -> return x
    _ -> MaybeT $ return Nothing

resolution'word'parsed ::
  HMap.HashMap Identifier Declarations ->
  BSC8.ByteString ->
  BSC8.ByteString ->
  Maybe SourceSpan
resolution'word'parsed rm q1 w1 = do
  mQ <- HMap.lookup q1 rm
  d <- HMap.lookup w1 $ resolution'simplify mQ
  Just $ _declSrcOrig d

resolution'word'parsed'b ::
  HMap.HashMap Identifier (HMap.HashMap Identifier Declaration) ->
  BSC8.ByteString ->
  BSC8.ByteString ->
  Maybe SourceSpan
resolution'word'parsed'b rm q1 w1 = do
  mQ <- HMap.lookup q1 rm
  d <- HMap.lookup w1 mQ
  Just $ _declSrcOrig d