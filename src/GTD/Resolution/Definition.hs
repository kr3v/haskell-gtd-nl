{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module GTD.Resolution.Definition where

import Control.Lens (use)
import Control.Monad (forM_)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadState (..))
import Control.Monad.Trans.Writer (execWriterT)
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import GTD.Cabal (ModuleNameS)
import qualified GTD.Haskell.Declaration as Declarations
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Identifier, Imports (..), asDeclsMap, hasNonEmptyOrig)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..))
import qualified GTD.Haskell.Module as HsModule
import GTD.Resolution.State (Package (Package, _modules))
import qualified GTD.Resolution.State as Package
import GTD.Utils (logErrorNSS, mapFrom)
import Text.Printf (printf)

