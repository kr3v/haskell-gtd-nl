{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

module GTD.Haskell.Module where

import Control.Exception (IOException, try)
import Control.Lens (makeLenses)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.State (MonadIO (..), forM_)
import Control.Monad.Trans.Writer (execWriterT)
import Data.Either.Combinators (mapLeft)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS, PackageNameS)
import GTD.Haskell.AST (Declarations, haskellGetIdentifiers, haskellParse)
import qualified GTD.Haskell.AST as Declarations
import GTD.Haskell.Cpphs (haskellApplyCppHs)
import GTD.Haskell.Declaration (Declaration, Identifier)
import GTD.Utils (logDebugNSS)
import Language.Haskell.Exts (Module (Module), SrcSpan (..), SrcSpanInfo (..))
import Text.Printf (printf)

data HsModule = HsModule
  { _package :: PackageNameS,
    _name :: ModuleNameS,
    _path :: FilePath,
    --
    _ast :: Module SrcSpanInfo,
    _deps :: [ModuleNameS],
    --
    _exports :: Map.Map Identifier Declaration,
    _decls :: Declarations
  }
  deriving (Show, Eq, Generic)

$(makeLenses ''HsModule)

emptySrcSpan :: SrcSpan
emptySrcSpan = SrcSpan {srcSpanFilename = "", srcSpanStartLine = 0, srcSpanStartColumn = 0, srcSpanEndLine = 0, srcSpanEndColumn = 0}

emptySrcSpanInfo :: SrcSpanInfo
emptySrcSpanInfo = SrcSpanInfo {srcInfoSpan = emptySrcSpan, srcInfoPoints = []}

emptyHaskellModule :: Module SrcSpanInfo
emptyHaskellModule = Module emptySrcSpanInfo Nothing [] [] []

emptyHsModule :: HsModule
emptyHsModule =
  HsModule
    { _package = "",
      _name = "",
      _path = "",
      _ast = emptyHaskellModule,
      _deps = [],
      _exports = Map.empty,
      _decls = mempty
    }

parseModule :: HsModule -> (MonadLoggerIO m, MonadError String m) => m HsModule
parseModule cm = do
  let srcP = _path cm
  let logTag = printf "parsing module %s" srcP
  logDebugNSS logTag $ printf ""

  src <- liftEither . mapLeft show =<< (liftIO (try $ readFile srcP) :: (MonadIO m) => m (Either IOException String))
  srcPostCpp <- haskellApplyCppHs srcP src
  ast <- liftEither $ haskellParse srcP srcPostCpp

  locals <- execWriterT $ haskellGetIdentifiers ast
  logDebugNSS logTag "locals:"
  forM_ (Declarations._decls locals) $ \i -> logDebugNSS logTag $ printf "\t%s" (show i)

  return $ cm {_ast = ast, _decls = locals}
