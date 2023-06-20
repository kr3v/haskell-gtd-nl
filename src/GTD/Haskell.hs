{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Haskell where

import Control.Exception
import Control.Lens (At (..), makeLenses, use, (%=), (^.))
import Control.Monad (forM, forM_, guard, unless, when)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Writer (WriterT (..), execWriterT, lift, tell)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.ByteString.UTF8 (fromString)
import Data.Data (Data (..), showConstr)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GTD.Cabal (CabalPackage (..), ModuleNameS, PackageNameS, cabalPackageName, haskellPath)
import GTD.Utils (maybeToMaybeT)
import Language.Haskell.Exts (Decl (..), ExportSpec (..), ExportSpecList (..), ImportDecl (..), ImportSpec (..), ImportSpecList (..), Module (..), ModuleHead (..), ModuleName (..), Name (..), ParseMode (..), ParseResult (..), QName (..), SrcSpan (..), SrcSpanInfo (..), defaultParseMode, parseFile, parseFileContents, parseFileContentsWithMode, parseFileWithCommentsAndPragmas, prettyPrint)
import Language.Haskell.Exts.Extension (Language (..))
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Preprocessor.Cpphs (defaultCpphsOptions, runCpphs)
import Text.Printf (printf)

haskellApplyCppHs :: FilePath -> String -> IO String
haskellApplyCppHs = runCpphs defaultCpphsOptions

haskellParse :: FilePath -> String -> Either String (Module SrcSpanInfo)
haskellParse src content = case parseFileContentsWithMode defaultParseMode {parseFilename = src, baseLanguage = Haskell2010} content of
  ParseOk m -> Right m
  ParseFailed loc e -> Left $ printf "failed to parse %s: %s @ %s\n" src e (show loc)

identToDecl ::
  ModuleName SrcSpanInfo ->
  Name SrcSpanInfo ->
  Bool ->
  Declaration
identToDecl (ModuleName _ mn) (Ident l n) isDeclaration =
  Declaration
    { declSrcUsage = if isDeclaration then emptySourceSpan else l',
      declSrcOrig = if isDeclaration then l' else emptySourceSpan,
      declName = n,
      declModule = mn
    }
  where
    l' = sourceSpan . srcInfoSpan $ l
-- TODO: deduplicate code
identToDecl (ModuleName _ mn) (Symbol l n) isDeclaration =
  Declaration
    { declSrcUsage = if isDeclaration then emptySourceSpan else l',
      declSrcOrig = if isDeclaration then l' else emptySourceSpan,
      declName = n,
      declModule = mn
    }
  where
    l' = sourceSpan . srcInfoSpan $ l

haskellGetIdentifiers :: Module SrcSpanInfo -> IO [Declaration]
haskellGetIdentifiers m = execWriterT (haskellGetIdentifiersW m)

haskellGetIdentifiersW :: Module SrcSpanInfo -> WriterT [Declaration] IO ()
haskellGetIdentifiersW m = do
  let (Module src head wtf1 imports decls) = m
  forM_ head $ \h -> do
    let (ModuleHead _ mN _ _) = h
    forM_ decls $ \case
      TypeSig _ names _ ->
        tell $ (\n -> identToDecl mN n True) <$> names
      _ -> return ()

haskellGetExportedIdentifiers :: Module SrcSpanInfo -> WriterT [Declaration] IO Bool
haskellGetExportedIdentifiers m = do
  let (Module src head wtf1 imports decls) = m
  forM_ head $ \h -> do
    let (ModuleHead _ mN _ mE) = h
    forM_ mE $ \(ExportSpecList _ es) -> do
      forM_ es $ \e -> do
        case e of
          EVar _ n -> case n of
            UnQual _ n -> tell [identToDecl mN n False]
            _ -> lift $ printf "haskellGetExportedIdentifiersW: not yet handled: %s -> %s\n" (show e) (show n)
          _ -> lift $ printf "haskellGetExportedIdentifiersW: not yet handled: %s\n" (show e)
  return $ isJust head

haskellGetImportedIdentifiers :: Module SrcSpanInfo -> WriterT [Declaration] IO ()
haskellGetImportedIdentifiers m = do
  let (Module src head wtf1 imports decls) = m
  forM_ imports $ \(ImportDecl {importModule = im, importSpecs = ss}) -> do
    lift $ print im
    case ss of
      Just (ImportSpecList _ isHidden is) -> do
        forM_ is $ \i -> do
          case i of
            IVar _ (Ident l n) -> tell [identToDecl im (Ident l n) False]
            _ -> return () -- lift $ printf "haskellGetImportedIdentifiers: not yet handled: %s\n" (show i)
          lift $ printf "\t%s\n" (show i)
      x -> lift $ printf ":t = %s\n" (showConstr . toConstr $ x)

data SourceSpan = SourceSpan
  { sourceSpanFileName :: FilePath,
    sourceSpanStartLine :: Int,
    sourceSpanStartColumn :: Int,
    sourceSpanEndLine :: Int,
    sourceSpanEndColumn :: Int
  }
  deriving (Show, Generic, Eq, Ord)

sourceSpan :: SrcSpan -> SourceSpan
sourceSpan (SrcSpan {srcSpanFilename = fileName, srcSpanStartLine = startLine, srcSpanStartColumn = startColumn, srcSpanEndLine = endLine, srcSpanEndColumn = endColumn}) =
  SourceSpan
    { sourceSpanFileName = fileName,
      sourceSpanStartLine = startLine,
      sourceSpanStartColumn = startColumn,
      sourceSpanEndLine = endLine,
      sourceSpanEndColumn = endColumn
    }

emptySourceSpan :: SourceSpan
emptySourceSpan = SourceSpan "" 1 1 1 1

data Declaration = Declaration
  { declSrcUsage :: SourceSpan,
    declSrcOrig :: SourceSpan,
    declModule :: ModuleNameS,
    declName :: String
  }
  deriving (Show, Eq, Generic, Ord)

instance FromJSON SourceSpan

instance FromJSON Declaration

instance ToJSON SourceSpan

instance ToJSON Declaration

data Identifier
  = Identifier String
  | QualifiedIdentifier String String
  deriving (Show, Eq, Ord)

data ContextModule = ContextModule
  { c2_module :: Module SrcSpanInfo,
    c2_exports :: Map.Map Identifier Declaration,
    c2_identifiers :: Map.Map Identifier Declaration
  }
  deriving (Show, Eq, Generic)

data ContextCabalPackage = ContextCabalPackage
  { _modules :: Map.Map PackageNameS (Map.Map ModuleNameS ContextModule),
    _dependencies :: [CabalPackage]
  }
  deriving (Show, Generic)

$(makeLenses ''ContextCabalPackage)

parsePackages :: StateT ContextCabalPackage IO ()
parsePackages = do
  deps <- use dependencies
  forM_ deps $ \dep -> parsePackage dep

parsePackage :: CabalPackage -> StateT ContextCabalPackage IO ()
parsePackage p = do
  mods <- use modules
  unless ((p ^. cabalPackageName) `Map.member` mods) $ do
    let mods = _cabalPackageExportedModules p
    let root = _cabalPackagePath p
    let srcDirs = _cabalPackageSrcDirs p
    forM_ (Map.assocs mods) $ \(modS, mod) -> do
      forM_ srcDirs $ \srcDir -> do
        let srcP = haskellPath root srcDir mod
        r <- parseModule False srcP
        case r of
          Left e -> lift $ printf "parseModule: %s\n" e
          Right c2 -> do
            m0 <- use modules
            lift $ printf "parsed %s! updating the map: %d\n" srcP (length m0)
            modules %= Map.insertWith Map.union (_cabalPackageName p) (Map.singleton modS c2)
            m1 <- use modules
            lift $ printf "parsed %s! updating the map: %d\n" srcP (length m1)
            return ()

enrich :: Declaration -> StateT ContextCabalPackage IO Declaration
enrich d = do
  deps <- use dependencies
  mods <- use modules
  xs' <- forM deps $ \dep -> do
    xm <- runMaybeT $ do
      guard $ declModule d `Map.member` _cabalPackageExportedModules dep
      dependencyModules <- maybeToMaybeT $ _cabalPackageName dep `Map.lookup` mods
      mod <- maybeToMaybeT $ declModule d `Map.lookup` dependencyModules
      decl <- maybeToMaybeT $ Map.lookup (Identifier $ declName d) (c2_exports mod)
      lift $ lift $ printf "enrich: updating %s with %s\n" (show d) (show decl)
      return $ d {declSrcOrig = declSrcOrig decl}
    let x = fromMaybe d xm
    when (x /= d) $ lift $ print x
    return x
  let xs = filter (\x -> sourceSpanFileName (declSrcOrig x) /= "") xs'
  case length xs of
    0 -> return d
    1 -> return $ head xs
    _ -> do
      lift $ printf "enrich: multiple matches for %s: %s\n" (show d) (show xs)
      return $ head xs

parseModule :: Bool -> FilePath -> StateT ContextCabalPackage IO (Either String ContextModule)
parseModule shouldEnrich srcP = do
  lift $ printf "parsing %s ...\n" srcP
  srcE <- lift (try $ readFile srcP) :: StateT ContextCabalPackage IO (Either IOException String)
  case srcE of
    Left e -> return $ Left $ show e
    Right src -> do
      src' <- lift $ haskellApplyCppHs srcP src
      case haskellParse srcP src' of
        Left e -> return $ Left e
        Right mod -> do
          locals <- lift $ haskellGetIdentifiers mod
          imports <- lift $ execWriterT $ haskellGetImportedIdentifiers mod
          imports' <- if shouldEnrich then forM imports enrich else return imports

          lift $ putStrLn "\n\n\n\n\n\n\n\n\n"

          lift $ printf "imports:\n"
          forM_ imports $ \i -> lift $ printf "\t%s\n" (show i)
          lift $ printf "imports':\n"
          forM_ imports' $ \i -> lift $ printf "\t%s\n" (show i)

          (isImplicitExportAll, exports) <- lift $ runWriterT $ haskellGetExportedIdentifiers mod

          let asDeclsMap ds = Map.fromList $ (\d -> (Identifier $ declName d, d)) <$> ds
              localsPlusImports = asDeclsMap $ locals ++ imports'
              exportsM' = asDeclsMap exports
              exportsM = if isImplicitExportAll then asDeclsMap locals else Map.intersection exportsM' localsPlusImports

          return $ Right (ContextModule {c2_module = mod, c2_exports = exportsM, c2_identifiers = localsPlusImports})