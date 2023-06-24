{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module GTD.Haskell.AST where

import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Writer (MonadWriter (tell), forM_, unless)
import Data.Data (Data (toConstr), showConstr)
import Data.Maybe (isJust, isNothing)
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS)
import GTD.Haskell.Declaration (Declaration, identToDecl)
import GTD.Utils (logDebugNSS)
import Language.Haskell.Exts (Decl (TypeSig), ExportSpec (EModuleContents, EVar), ExportSpecList (ExportSpecList), Extension (EnableExtension), ImportDecl (..), ImportSpec (IVar), ImportSpecList (ImportSpecList), KnownExtension (CPP), Language (Haskell2010), Module (Module), ModuleHead (ModuleHead), ModuleName (ModuleName), Name (..), ParseMode (..), ParseResult (ParseFailed, ParseOk), QName (UnQual), SrcSpanInfo, defaultParseMode, parseFileContentsWithMode)
import Text.Printf (printf)

-- TODO: figure out #line pragmas
haskellParse :: FilePath -> String -> Either String (Module SrcSpanInfo)
haskellParse src content = case parseFileContentsWithMode defaultParseMode {parseFilename = src, baseLanguage = Haskell2010} content of
  ParseOk m -> Right m
  ParseFailed loc e -> Left $ printf "failed to parse %s: %s @ %s" src e (show loc)

haskellGetIdentifiers :: Module SrcSpanInfo -> (MonadWriter [Declaration] m, MonadLoggerIO m) => m ()
haskellGetIdentifiers m = do
  let (Module src head wtf1 imports decls) = m
  forM_ head $ \h -> do
    let (ModuleHead _ mN _ _) = h
    forM_ decls $ \case
      TypeSig _ names _ ->
        tell $ (\n -> identToDecl mN n True) <$> names
      _ -> return ()

data Exports = Exports
  { exports :: [Declaration],
    reexports :: [ModuleNameS]
  }
  deriving (Show, Generic, Eq)

instance Semigroup Exports where
  (<>) :: Exports -> Exports -> Exports
  (<>) (Exports es1 rs1) (Exports es2 rs2) = Exports (es1 <> es2) (rs1 <> rs2)

instance Monoid Exports where
  mempty :: Exports
  mempty = Exports [] []

haskellGetExports :: Module SrcSpanInfo -> (MonadWriter Exports m, MonadLoggerIO m) => m Bool
haskellGetExports m = do
  let logTag = "get exports"
  let (Module src head wtf1 imports decls) = m
  forM_ head $ \h -> do
    let (ModuleHead _ mN _ mE) = h
    forM_ mE $ \(ExportSpecList _ es) ->
      forM_ es $ \e -> case e of
        EVar _ n -> case n of
          UnQual _ n -> tell mempty {exports = [identToDecl mN n False]}
          _ -> logDebugNSS logTag $ printf "not yet handled: %s -> %s" (show e) (show n)
        EModuleContents _ (ModuleName _ mn) -> tell mempty {reexports = [mn]}
        _ -> logDebugNSS logTag $ printf "not yet handled: %s" (show e)
  return $ isNothing head

data Imports = Imports
  { imports :: [Declaration],
    importedModules :: [ModuleNameS]
  }
  deriving (Show, Generic, Eq)

instance Semigroup Imports where
  (<>) :: Imports -> Imports -> Imports
  (<>) (Imports is1 ims1) (Imports is2 ims2) = Imports (is1 <> is2) (ims1 <> ims2)

instance Monoid Imports where
  mempty :: Imports
  mempty = Imports [] []

haskellGetImports :: Module SrcSpanInfo -> (MonadWriter Imports m, MonadLoggerIO m) => m ()
haskellGetImports m = do
  let logTag = "get imported identifiers"
  let (Module src head wtf1 imports decls) = m
  forM_ imports $ \(ImportDecl {importModule = im@(ModuleName _ imn), importQualified = iq, importSrc = isr, importSafe = isa, importPkg = ip, importAs = ia, importSpecs = ss}) -> do
    unless (iq || isr || isa || isJust ia || isJust ip) $ do
      logDebugNSS logTag $ printf "handling module=%s (:t(ss) == %s)" (show im) (showConstr $ toConstr ss)
      case ss of
        Just (ImportSpecList _ isHidden is) -> do
          forM_ is $ \i -> do
            case i of
              IVar _ (Ident l n) -> tell mempty {imports = [identToDecl im (Ident l n) False]}
              IVar _ (Symbol l n) -> tell mempty {imports = [identToDecl im (Ident l n) False]}
              _ -> logDebugNSS logTag $ printf "haskellGetImports: not yet handled: %s" (show i)
            logDebugNSS logTag $ printf "\t%s" (show i)
        Nothing -> tell mempty {importedModules = [imn]}

haskellGetReexportedModules :: Module SrcSpanInfo -> (MonadWriter [ModuleNameS] m, MonadLoggerIO m) => m ()
haskellGetReexportedModules m = do
  let logTag = "get re-exported modules"
  let (Module src head wtf1 imports decls) = m
  forM_ head $ \h -> do
    let (ModuleHead _ mN _ mE) = h
    forM_ mE $ \(ExportSpecList _ es) ->
      forM_ es $ \case
        EModuleContents _ (ModuleName _ n) -> tell [n]
        _ -> return ()

haskellGetImportedModules :: Module SrcSpanInfo -> (MonadWriter [ModuleNameS] m, MonadLoggerIO m) => m ()
haskellGetImportedModules m = do
  let logTag = "get imported modules"
  let (Module src head wtf1 imports decls) = m
  forM_ imports $ \(ImportDecl {importModule = (ModuleName _ mn), importSpecs = ss}) -> do
    tell [mn]