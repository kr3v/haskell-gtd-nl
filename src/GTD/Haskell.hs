{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module GTD.Haskell where

import Control.Monad (forM_)
import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.UTF8 (fromString)
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Language.Haskell.Exts (Decl (..), ExportSpec (..), ExportSpecList (..), Module (..), ModuleHead (..), Name (..), ParseMode (..), ParseResult (..), QName (..), SrcSpan (..), SrcSpanInfo (..), defaultParseMode, parseFile, parseFileContents, parseFileContentsWithMode, parseFileWithCommentsAndPragmas, prettyPrint)
import Language.Haskell.Exts.Extension (Language (..))
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Preprocessor.Cpphs (defaultCpphsOptions, runCpphs)
import Text.Printf (printf)
import Data.Maybe (isJust)

haskellApplyCppHs :: FilePath -> String -> IO String
haskellApplyCppHs = runCpphs defaultCpphsOptions

haskellParse :: FilePath -> String -> Either String (Module SrcSpanInfo)
haskellParse src content = case parseFileContentsWithMode defaultParseMode {parseFilename = src, baseLanguage = Haskell2010} content of
  ParseOk m -> Right m
  ParseFailed loc e -> Left $ printf "failed to parse %s: %s @ %s" src e (show loc)

data SourceSpan = SourceSpan
  { sourceSpanFileName :: String,
    sourceSpanStartLine :: Int,
    sourceSpanStartColumn :: Int,
    sourceSpanEndLine :: Int,
    sourceSpanEndColumn :: Int
  }
  deriving (Show, Generic, Eq)

sourceSpan :: SrcSpan -> SourceSpan
sourceSpan (SrcSpan {srcSpanFilename = fileName, srcSpanStartLine = startLine, srcSpanStartColumn = startColumn, srcSpanEndLine = endLine, srcSpanEndColumn = endColumn}) =
  SourceSpan
    { sourceSpanFileName = fileName,
      sourceSpanStartLine = startLine,
      sourceSpanStartColumn = startColumn,
      sourceSpanEndLine = endLine,
      sourceSpanEndColumn = endColumn
    }

data Declaration = Declaration
  { declarationSrcSpan :: SourceSpan,
    declarationName :: String
  }
  deriving (Show, Generic, Eq)

instance FromJSON SourceSpan

instance FromJSON Declaration

instance ToJSON SourceSpan

instance ToJSON Declaration

-- TODO: use something to add an ability to perform a lookup?
type Declarations = [Declaration]

haskellGetIdentifiers :: Module SrcSpanInfo -> IO [Declaration]
haskellGetIdentifiers m = execWriterT (haskellGetIdentifiersW m)

haskellGetIdentifiersW :: Module SrcSpanInfo -> WriterT [Declaration] IO ()
haskellGetIdentifiersW m = do
  let (Module src head wtf1 imports decls) = m
  forM_ decls $ \case
    TypeSig _ names _ ->
      tell $ (\(Ident l n) -> Declaration (sourceSpan . srcInfoSpan $ l) n) <$> names
    _ -> return ()

-- TODO: handle more cases
haskellGetExportedIdentifiers :: Module SrcSpanInfo -> WriterT [Declaration] IO Bool
haskellGetExportedIdentifiers m = do
  let (Module src head wtf1 imports decls) = m
  forM_ head $ \h -> do
    let (ModuleHead _ _ _ mE) = h
    forM_ mE $ \(ExportSpecList _ es) -> do
      forM_ es $ \e -> do
        case e of
          EVar _ n -> case n of
            UnQual _ (Ident l n) -> tell [Declaration (sourceSpan . srcInfoSpan $ l) n]
            _ -> lift $ printf "haskellGetExportedIdentifiersW: not yet handled: %s" (show e)
          _ -> lift $ printf "haskellGetExportedIdentifiersW: not yet handled: %s" (show e)
  return $ isJust head

-- haskellGetImportedIdentifiers :: Module SrcSpanInfo -> IO ()
-- haskellGetImportedIdentifiers m = do
--   let (Module src head wtf1 imports decls) = m
--   forM_ imports $ \(ImportDecl {importModule = m, importSpecs = ss}) -> do
--     print m
--     case ss of
--       Just (ImportSpecList _ isHidden is) -> do
--         forM_ is \i -> do
--       _ -> return ()
--   print "<<<<***>>>>\n"