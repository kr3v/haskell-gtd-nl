{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module GTD.Haskell.AST where

import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Writer (MonadWriter (tell), forM_)
import Data.Data (Data (toConstr), showConstr)
import Data.Maybe (isJust)
import GTD.Cabal (ModuleNameS)
import GTD.Haskell.Declaration (Declaration, identToDecl)
import GTD.Utils (logDebugNSS)
import Language.Haskell.Exts
  ( Decl (TypeSig),
    ExportSpec (EModuleContents, EVar),
    ExportSpecList (ExportSpecList),
    ImportDecl (ImportDecl, importModule, importSpecs),
    ImportSpec (IVar),
    ImportSpecList (ImportSpecList),
    Language (Haskell2010),
    Module (Module),
    ModuleHead (ModuleHead),
    ModuleName (ModuleName),
    Name (Ident),
    ParseMode (baseLanguage, parseFilename),
    ParseResult (ParseFailed, ParseOk),
    QName (UnQual),
    SrcSpanInfo,
    defaultParseMode,
    parseFileContentsWithMode,
  )
import Text.Printf (printf)

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

haskellGetExportedIdentifiers :: Module SrcSpanInfo -> (MonadWriter [Declaration] m, MonadLoggerIO m) => m Bool
haskellGetExportedIdentifiers m = do
  let logTag = "get exported identifiers"
  let (Module src head wtf1 imports decls) = m
  forM_ head $ \h -> do
    let (ModuleHead _ mN _ mE) = h
    forM_ mE $ \(ExportSpecList _ es) ->
      forM_ es $ \e -> case e of
        EVar _ n -> case n of
          UnQual _ n -> tell [identToDecl mN n False]
          _ -> logDebugNSS logTag $ printf "not yet handled: %s -> %s" (show e) (show n)
        _ -> logDebugNSS logTag $ printf "not yet handled: %s" (show e)
  return $ isJust head

haskellGetImportedIdentifiers :: Module SrcSpanInfo -> (MonadWriter [Declaration] m, MonadLoggerIO m) => m ()
haskellGetImportedIdentifiers m = do
  let logTag = "get imported identifiers"
  let (Module src head wtf1 imports decls) = m
  forM_ imports $ \(ImportDecl {importModule = im, importSpecs = ss}) -> do
    logDebugNSS logTag $ show im
    case ss of
      Just (ImportSpecList _ isHidden is) -> do
        forM_ is $ \i -> do
          case i of
            IVar _ (Ident l n) -> tell [identToDecl im (Ident l n) False]
            _ -> logDebugNSS logTag $ printf "haskellGetImportedIdentifiers: not yet handled: %s" (show i)
          logDebugNSS logTag $ printf "\t%s" (show i)
      x -> logDebugNSS logTag $ printf ":t = %s" (showConstr . toConstr $ x)

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