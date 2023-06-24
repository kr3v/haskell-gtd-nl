{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module GTD.Haskell.AST where

import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Writer (MonadWriter (tell), Writer, execWriter, forM_, unless)
import Data.Data (Data (toConstr), showConstr)
import Data.Maybe (isJust, isNothing)
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS)
import GTD.Haskell.Declaration (Declaration, identToDecl)
import GTD.Utils (logDebugNSS)
import Language.Haskell.Exts (Decl (TypeSig), ExportSpec (EModuleContents, EVar), ExportSpecList (ExportSpecList), ImportDecl (..), ImportSpec (IVar), ImportSpecList (ImportSpecList), Language (Haskell2010), Module (Module), ModuleHead (ModuleHead), ModuleHeadAndImports (..), ModuleName (ModuleName), Name (..), NonGreedy (..), ParseMode (..), ParseResult (ParseFailed, ParseOk), Parseable (..), QName (UnQual), SrcSpanInfo, defaultParseMode, infix_, parseFileContentsWithMode)
import Text.Printf (printf)

haskellGetImportedSymbols :: [ImportDecl SrcSpanInfo] -> Writer [String] ()
haskellGetImportedSymbols imports =
  forM_ imports $ \(ImportDecl {importQualified = iq, importSrc = isr, importSafe = isa, importPkg = ip, importAs = ia, importSpecs = ss}) -> do
    unless (iq || isr || isa || isJust ia || isJust ip) $ do
      case ss of
        Just (ImportSpecList _ False is) -> forM_ is $ \s -> do
          case s of
            IVar _ (Symbol _ n) -> tell [n]
            _ -> return ()
        _ -> return ()

haskellDropPragmas :: String -> String
haskellDropPragmas c = unlines ((\l -> if not (null l) && head l == '#' then "" else l) <$> lines c)

-- in this case, the proposal is to get all the imported operators and explicitly set a fixity for them
-- though this is the wrong place to do it, since imports might be unspecified
haskellParseAmbigousInfixOperators :: FilePath -> String -> Either String (Module SrcSpanInfo)
haskellParseAmbigousInfixOperators src content = do
  let ei = parse (haskellDropPragmas content) :: ParseResult (NonGreedy (ModuleHeadAndImports SrcSpanInfo))
  case ei of
    ParseFailed loc e -> Left $ printf "failed to parse %s: %s @ %s" src e (show loc)
    ParseOk (NonGreedy (ModuleHeadAndImports _ _ _ is)) -> do
      let operators = execWriter $ haskellGetImportedSymbols is
      let fixs = infix_ 0 operators
      case parseFileContentsWithMode defaultParseMode {parseFilename = src, baseLanguage = Haskell2010, fixities = Just fixs} content of
        ParseOk m -> Right m
        ParseFailed loc e -> Left $ printf "failed to parse %s: %s @ %s" src e (show loc)

-- TODO: figure out #line pragmas
haskellParse :: FilePath -> String -> Either String (Module SrcSpanInfo)
haskellParse src content = case parseFileContentsWithMode defaultParseMode {parseFilename = src, baseLanguage = Haskell2010} content of
  ParseOk m -> Right m
  ParseFailed loc e -> case e of
    "Ambiguous infix expression" -> haskellParseAmbigousInfixOperators src content
    _ -> Left $ printf "failed to parse %s: %s @ %s" src e (show loc)

haskellGetIdentifiers :: Module SrcSpanInfo -> (MonadWriter [Declaration] m, MonadLoggerIO m) => m ()
haskellGetIdentifiers (Module _ mhead _ _ decls) =
  forM_ mhead $ \h -> do
    let (ModuleHead _ mN _ _) = h
    forM_ decls $ \case
      TypeSig _ names _ ->
        tell $ (\n -> identToDecl mN n True) <$> names
      _ -> return ()
haskellGetIdentifiers m = logDebugNSS "get identifiers" (printf "not yet handled: :t m = %s" (showConstr . toConstr $ m))

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
haskellGetExports (Module _ mhead _ _ _) = do
  let logTag = "get exports"
  forM_ mhead $ \h -> do
    let (ModuleHead _ mN _ mE) = h
    forM_ mE $ \(ExportSpecList _ es) ->
      forM_ es $ \e -> case e of
        EVar _ qn -> case qn of
          UnQual _ n -> tell mempty {exports = [identToDecl mN n False]}
          _ -> logDebugNSS logTag $ printf "not yet handled: %s -> %s" (show e) (show qn)
        EModuleContents _ (ModuleName _ mn) -> tell mempty {reexports = [mn]}
        _ -> logDebugNSS logTag $ printf "not yet handled: %s" (show e)
  return $ isNothing mhead
haskellGetExports m = logDebugNSS "get exports" (printf "not yet handled: :t m = %s" (showConstr . toConstr $ m)) >> return False

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
haskellGetImports (Module _ _ _ is _) = do
  let logTag = "get imports"
  forM_ is $ \(ImportDecl {importModule = im@(ModuleName _ imn), importQualified = iq, importSrc = isr, importSafe = isa, importPkg = ip, importAs = ia, importSpecs = ss}) -> do
    unless (iq || isr || isa || isJust ia || isJust ip) $ do
      logDebugNSS logTag $ printf "handling module=%s (:t(ss) == %s)" (show im) (showConstr $ toConstr ss)
      case ss of
        Just (ImportSpecList _ False is') -> do
          forM_ is' $ \i -> do
            case i of
              IVar _ (Ident l n) -> tell mempty {imports = [identToDecl im (Ident l n) False]}
              IVar _ (Symbol l n) -> tell mempty {imports = [identToDecl im (Ident l n) False]}
              _ -> logDebugNSS logTag $ printf "not yet handled: %s" (show i)
            logDebugNSS logTag $ printf "\t%s" (show i)
        Just (ImportSpecList _ True _) -> logDebugNSS logTag $ printf "not yet handled: isHidden is `True`"
        Nothing -> tell mempty {importedModules = [imn]}
haskellGetImports m = logDebugNSS "get imports" $ printf "not yet handled: :t m = %s" (showConstr . toConstr $ m)
