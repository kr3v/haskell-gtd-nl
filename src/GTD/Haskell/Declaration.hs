{-# LANGUAGE DeriveGeneric #-}

module GTD.Haskell.Declaration where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS)
import Language.Haskell.Exts (ModuleName (..), Name (..), SrcSpan (..), SrcSpanInfo (srcInfoSpan))

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

hasNonEmptyOrig :: Declaration -> Bool
hasNonEmptyOrig = (/= emptySourceSpan) . _declSrcOrig

emptySourceSpan :: SourceSpan
emptySourceSpan = SourceSpan "" 0 0 0 0

data Declaration = Declaration
  { _declSrcUsage :: SourceSpan,
    _declSrcOrig :: SourceSpan,
    _declModule :: ModuleNameS,
    _declName :: String
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

identToDecl :: ModuleName SrcSpanInfo -> Name SrcSpanInfo -> Bool -> Declaration
identToDecl m (Symbol l n) = identToDecl' m l n
identToDecl m (Ident l n) = identToDecl' m l n

identToDecl' ::
  ModuleName SrcSpanInfo ->
  SrcSpanInfo ->
  String ->
  Bool ->
  Declaration
identToDecl' (ModuleName _ mn) l n isDeclaration =
  Declaration
    { _declSrcUsage = if isDeclaration then emptySourceSpan else l',
      _declSrcOrig = if isDeclaration then l' else emptySourceSpan,
      _declName = n,
      _declModule = mn
    }
  where
    l' = sourceSpan . srcInfoSpan $ l