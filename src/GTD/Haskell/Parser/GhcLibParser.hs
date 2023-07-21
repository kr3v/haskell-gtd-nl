{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module GTD.Haskell.Parser.GhcLibParser where

import Control.Monad (forM_)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Writer (MonadWriter (..))
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Errors.Types (DriverMessageOpts (psDiagnosticOpts))
import GHC.Hs (GhcPs, HsDecl (..), HsModule (..), Sig (..), SrcSpanAnn' (..))
import GHC.Parser (parseModule)
import GHC.Parser.Errors.Types (PsMessage (..))
import GHC.Parser.Lexer (P (unP), PState (..), ParseResult (..), ParserOpts, initParserState, mkParserOpts)
import GHC.Types.Error (DecoratedSDoc, Diagnostic (..))
import GHC.Types.SrcLoc (GenLocated (..), RealSrcSpan (srcSpanFile), SrcSpan (..), mkRealSrcLoc, srcSpanStartLine, srcSpanEndLine, srcSpanEndCol, srcSpanStartCol)
import GHC.Utils.Error (DiagOpts (..), pprMessages)
import GHC.Utils.Outputable (Outputable (..), SDocContext (..), defaultSDocContext, renderWithContext)
import GTD.Haskell.Declaration (Declaration (..), Declarations (..), SourceSpan (..), asDeclsMap)

showO :: Outputable a => a -> String
showO = renderWithContext defaultSDocContext {sdocErrorSpans = True} . ppr

showS :: Outputable a => SrcSpanAnn' a -> String
showS (SrcSpanAnn ann loc) = showO ann ++ " " ++ showO loc

ghcParse :: FilePath -> ParserOpts -> String -> P a -> ParseResult a
ghcParse filename opts str parser = unP parser parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = initParserState opts buffer location

parse :: FilePath -> IO (HsModule GhcPs)
parse p = do
  content <- readFile p
  let diagOpts = DiagOpts EnumSet.empty EnumSet.empty False False Nothing defaultSDocContext
      opts = mkParserOpts EnumSet.empty diagOpts [] False False False False
      r = ghcParse p opts content parseModule
  case r of
    POk _ (L l e) -> return e
    PFailed s -> do
      fail (showO $ errors s)

data Definition = Definition
  { definitionIdentifier :: FilePath,
    definitionLocation :: SrcSpan
  }
  deriving (Show, Eq)

asSourceSpan :: SrcSpan -> SourceSpan
asSourceSpan (RealSrcSpan r _) = SourceSpan {
  sourceSpanFileName = unpackFS $ srcSpanFile r,
  sourceSpanStartLine = srcSpanStartLine r,
  sourceSpanStartColumn = srcSpanStartCol r,
  sourceSpanEndLine = srcSpanEndLine r,
  sourceSpanEndColumn = srcSpanEndCol r
  }

identifiers :: HsModule GhcPs -> (MonadWriter Declarations m, MonadLoggerIO m) => m ()
identifiers m = do
  forM_ (hsmodDecls m) $ \(L _ l) -> case l of
    SigD _ s -> do
      case s of
        TypeSig _ h i -> do
          forM_ h $ \(L (SrcSpanAnn ann loc) k) -> do
            tell mempty {_decls = asDeclsMap [Declaration {_declSrcOrig = asSourceSpan loc, _declModule = "", _declName = showO k}]}
        _ -> return ()
    TyClD g h -> return ()
    _ -> return ()
