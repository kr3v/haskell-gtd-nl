{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module GTD.Haskell.Parser.GhcLibParser where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.State (MonadState (..), evalStateT, execStateT, modify)
import Control.Monad.Writer (MonadWriter (..))
import Data.Maybe (mapMaybe)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Errors.Types (DriverMessageOpts (psDiagnosticOpts))
import GHC.Driver.Ppr (showSDoc)
import GHC.Driver.Session (DynFlags (..), Language (..), PlatformMisc (..), Settings (..), defaultDynFlags, initDynFlags, parseDynamicFilePragma, supportedLanguagesAndExtensions)
import GHC.Fingerprint (fingerprint0)
import GHC.Hs (GhcPs, HsDecl (..), HsModule (..), ModuleName (ModuleName), Sig (..), SrcSpanAnn' (..), TyClDecl (..))
import GHC.LanguageExtensions (Extension (..))
import GHC.Parser (parseModule)
import GHC.Parser.Errors.Types (PsMessage (..))
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer (P (unP), PState (..), ParseResult (..), ParserOpts, getPsMessages, initParserState, mkParserOpts)
import GHC.Platform (Arch (..), ArchOS (..), ByteOrder (..), OS (..), PlatformWordSize (..), genericPlatform)
import GHC.Settings (FileSettings (..), GhcNameVersion (..), Platform (..), ToolSettings (..), sTopDir)
import GHC.Settings.Config (cProjectVersion)
import GHC.Types.Error (DecoratedSDoc, Diagnostic (..), Messages (getMessages))
import GHC.Types.SourceError (handleSourceError, srcErrorMessages)
import GHC.Types.SrcLoc (GenLocated (..), Located, RealSrcSpan (srcSpanFile), SrcSpan (..), mkRealSrcLoc, srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)
import GHC.Utils.Error (DiagOpts (..), pprMessages, pprMsgEnvelopeBagWithLocDefault)
import GHC.Utils.Outputable (Outputable (..), SDocContext (..), defaultSDocContext, renderWithContext)
import GHC.Utils.Panic (handleGhcException)
import GTD.Haskell.Declaration (Declaration (..), Declarations (..), SourceSpan (..), asDeclsMap, name)
import GTD.Haskell.Parser.GhcLibParser.Extension (readExtension)
import GTD.Utils (modifyM)
import qualified Language.Haskell.Exts as HSE

showO :: Outputable a => a -> String
showO = renderWithContext defaultSDocContext {sdocErrorSpans = True} . ppr

showS :: Outputable a => SrcSpanAnn' a -> String
showS (SrcSpanAnn ann loc) = showO ann ++ " " ++ showO loc

ghcParse :: FilePath -> ParserOpts -> String -> ParseResult (Located (HsModule GhcPs))
ghcParse filename opts str = unP parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = initParserState opts buffer location

---

fakeSettings :: Settings
fakeSettings =
  Settings
    { sGhcNameVersion = GhcNameVersion {ghcNameVersion_programName = "ghc", ghcNameVersion_projectVersion = cProjectVersion},
      sFileSettings = FileSettings {},
      sTargetPlatform = genericPlatform,
      sPlatformMisc = PlatformMisc {},
      sToolSettings = ToolSettings {toolSettings_opt_P_fingerprint = fingerprint0}
    }

parsePragmasIntoDynFlags :: DynFlags -> FilePath -> String -> IO (Maybe DynFlags)
parsePragmasIntoDynFlags dynFlags p content = do
  let (_, opts) = getOptions (initParserOpts dynFlags) (stringToStringBuffer content) p
  (flags, _, _) <- parseDynamicFilePragma dynFlags opts
  return $ Just flags

parse :: FilePath -> String -> IO (Either String (HsModule GhcPs))
parse p content = do
  let dynFlags0 = defaultDynFlags fakeSettings
  dynFlags <-
    parsePragmasIntoDynFlags dynFlags0 p content >>= \case
      Nothing -> return dynFlags0
      Just flags -> return flags

  let opts = initParserOpts dynFlags
      location = mkRealSrcLoc (mkFastString p) 1 1
      buffer = stringToStringBuffer content
      parseState = initParserState opts buffer location
      r = unP parseModule parseState

  return $ case r of
    POk _ (L l e) -> Right e
    PFailed s -> Left $ showO $ errors s

---

asSourceSpan :: SrcSpan -> SourceSpan
asSourceSpan (RealSrcSpan r _) =
  SourceSpan
    { sourceSpanFileName = unpackFS $ srcSpanFile r,
      sourceSpanStartLine = srcSpanStartLine r,
      sourceSpanStartColumn = srcSpanStartCol r,
      sourceSpanEndLine = srcSpanEndLine r,
      sourceSpanEndColumn = srcSpanEndCol r
    }

identifiers :: HsModule GhcPs -> (MonadWriter Declarations m, MonadLoggerIO m) => m ()
identifiers m@(HsModule {hsmodName = Just (L _ (ModuleName nF))}) = do
  let mN = unpackFS nF
  forM_ (hsmodDecls m) $ \(L _ d) -> case d of
    SigD _ s -> case s of
      TypeSig _ h i -> do
        forM_ h $ \(L (SrcSpanAnn ann loc) k) -> do
          tell mempty {_decls = asDeclsMap [Declaration {_declSrcOrig = asSourceSpan loc, _declModule = mN, _declName = showO k}]}
      _ -> return ()
    TyClD _ tc -> case tc of
      SynDecl _ h n _ _ -> return ()
        -- tell mempty {_decls = asDeclsMap [Declaration {_declSrcOrig = asSourceSpan $ getLoc h, _declModule = mN, _declName = showO n}]}
    _ -> return ()
identifiers _ = return ()