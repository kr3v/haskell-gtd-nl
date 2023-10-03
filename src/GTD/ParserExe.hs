{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module GTD.ParserExe where

import Control.Lens (use)
import Control.Monad (forM_, void, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State (evalStateT)
import qualified GTD.Cabal.Cache as Cabal (load, store)
import GTD.Cabal.Types (Designation, Package (..))
import qualified GTD.Cabal.Types as Cabal (changed)
import GTD.Configuration (GTDConfiguration)
import GTD.Resolution.Package (package'resolution'withDependencies'concurrently)
import GTD.Server.Definition (cabalPackage'unresolved'plusStoreInLocals)
import GTD.State (ccGet, emptyContext)
import GTD.Utils (logErrorNSS)

parserExe ::
  GTDConfiguration ->
  FilePath ->
  Designation ->
  (MonadLoggerIO m, MonadBaseControl IO m) => m ()
parserExe constants wd des = do
  flip runReaderT constants $ flip evalStateT emptyContext $ do
    Cabal.load
    e <- runExceptT $ do
      cPkgsU <- cabalPackage'unresolved'plusStoreInLocals wd
      forM_ cPkgsU $ \p ->
        when (_designation p == des) $
          void $
            package'resolution'withDependencies'concurrently p
    ccGC <- use $ ccGet . Cabal.changed
    when ccGC Cabal.store
    case e of
      Left err -> void $ logErrorNSS wd err
      Right _ -> return ()