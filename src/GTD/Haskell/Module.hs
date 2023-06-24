{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Haskell.Module where

import Control.Exception (IOException, try)
import Control.Lens (makeLenses)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.State (MonadIO (..), forM_)
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Writer (execWriterT)
import Data.Either.Combinators (mapLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS, PackageNameS)
import GTD.Haskell.AST (haskellGetIdentifiers, haskellParse)
import GTD.Haskell.Cpphs (haskellApplyCppHs)
import GTD.Haskell.Declaration (Declaration, Identifier)
import GTD.Haskell.Utils (asDeclsMap)
import GTD.Utils (logDebugNSS)
import Language.Haskell.Exts (Module (Module), SrcSpan (..), SrcSpanInfo (..))
import Text.Printf (printf)

data HsModule = HsModule
  { _package :: PackageNameS,
    _name :: ModuleNameS,
    _path :: FilePath,
    _ast :: Module SrcSpanInfo,
    _deps :: [ModuleNameS],
    _exports :: Map.Map Identifier Declaration,
    _decls :: Map.Map Identifier Declaration
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
      _decls = Map.empty
    }

parseModule :: HsModule -> (MonadLoggerIO m) => ExceptT String m HsModule
parseModule cm = do
  let srcP = _path cm
  let logTag = printf "parsing module %s" srcP
  logDebugNSS logTag $ printf ""

  src <- ExceptT $ mapLeft show <$> (liftIO (try $ readFile srcP) :: (MonadIO m) => m (Either IOException String))
  srcPostCpp <- haskellApplyCppHs srcP src
  mod <- ExceptT $ return $ haskellParse srcP srcPostCpp

  locals <- execWriterT $ haskellGetIdentifiers mod
  logDebugNSS logTag "locals:"
  forM_ locals $ \i -> logDebugNSS logTag $ printf "\t%s" (show i)

  return $ cm {_ast = mod, _decls = asDeclsMap locals}
