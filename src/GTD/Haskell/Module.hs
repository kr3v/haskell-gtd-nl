{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Haskell.Module where

import Control.Exception (IOException, try)
import Control.Lens (makeLenses)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.State (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Either.Combinators (mapLeft)
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS, PackageNameS)
import GTD.Haskell.AST (Declarations, haskellParse)
import GTD.Haskell.Cpphs (haskellApplyCppHs)
import GTD.Utils (logDebugNSS)
import Language.Haskell.Exts (Module (Module), SrcSpan (..), SrcSpanInfo (..))
import Text.Printf (printf)

data HsModule = HsModule
  { _package :: PackageNameS,
    _name :: ModuleNameS,
    _path :: FilePath,
    _ast :: Module SrcSpanInfo
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
      _ast = emptyHaskellModule
    }

parseModule :: HsModule -> (MonadLoggerIO m, MonadError String m) => m HsModule
parseModule cm = do
  let srcP = _path cm
  let logTag = "parsing module " ++ srcP
  logDebugNSS logTag ""

  src <- liftEither . mapLeft show =<< (liftIO (try $ readFile srcP) :: (MonadIO m) => m (Either IOException String))
  srcPostCpp <- haskellApplyCppHs srcP src
  ast <- liftEither $ haskellParse srcP srcPostCpp

  return $ cm {_ast = ast}

---

newtype HsModuleP = HsModuleP
  { _exports :: Declarations
  }
  deriving (Show, Eq, Generic)

$(makeLenses ''HsModuleP)

instance FromJSON HsModuleP

instance ToJSON HsModuleP
