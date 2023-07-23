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
import qualified Data.Map.Strict as Map
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
import GHC.Hs (ConDecl (..), ConDeclField (..), DataDefnCons (..), FamilyDecl (..), FieldOcc (..), GhcPs, HsConDetails (..), HsDataDefn (..), HsDecl (..), HsModule (..), ModuleName (ModuleName), Sig (..), SrcSpanAnn' (..), TyClDecl (..))
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
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), SourceSpan (..), asDeclsMap, name)
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
  let decl loc k = Declaration {_declSrcOrig = asSourceSpan loc, _declModule = mN, _declName = showO k}
  let tellD loc k = tell mempty {_decls = asDeclsMap [decl loc k]}

  forM_ (hsmodDecls m) $ \(L _ d) -> case d of
    SigD _ s -> case s of
      TypeSig _ names _ -> do
        forM_ names $ \(L (SrcSpanAnn ann loc) k) -> tellD loc k
      _ -> return ()
    TyClD _ tc -> case tc of
      FamDecl {tcdFam = FamilyDecl {fdLName = (L (SrcSpanAnn ann loc) k)}} -> tellD loc k -- TODO: add more stuff?
      SynDecl {tcdLName = (L (SrcSpanAnn ann loc) k)} -> tellD loc k
      DataDecl {tcdLName = (L (SrcSpanAnn ann loc) k), tcdDataDefn = (HsDataDefn _ _ _ _ ctorsD _)} -> do
        let ctors = case ctorsD of
              NewTypeCon a -> [a]
              DataTypeCons _ as -> as
        let fs = flip concatMap ctors $ \(L _ ctor) -> case ctor of
              ConDeclH98 {con_name = (L (SrcSpanAnn ann loc1) k1), con_args = ctor1} -> do
                let fields = case ctor1 of
                      PrefixCon pc1 pc2 -> []
                      InfixCon ic1 ic2 -> []
                      RecCon (L _ fs1) -> flip concatMap fs1 $ \(L _ (ConDeclField _ fs2 _ _)) ->
                        flip fmap fs2 $ \(L _ (FieldOcc _ (L (SrcSpanAnn ann loc2) k2))) ->
                          decl loc2 k2
                fields ++ [decl loc1 k1]
              _ -> []
        tell mempty {_dataTypes = Map.singleton (showO k) ClassOrData {_cdtName = decl loc k, _cdtFields = asDeclsMap fs, _eWildcard = False}}
      ClassDecl {tcdLName = (L (SrcSpanAnn ann loc) k), tcdSigs = ms} -> do
        let fs = flip concatMap ms $ \(L _ m) -> case m of
              TypeSig _ names _ -> flip fmap names $ \(L (SrcSpanAnn ann loc) k) -> decl loc k
              ClassOpSig _ _ names _ -> flip fmap names $ \(L (SrcSpanAnn ann loc) k) -> decl loc k
              _ -> []
        tell mempty {_dataTypes = Map.singleton (showO k) ClassOrData {_cdtName = decl loc k, _cdtFields = asDeclsMap fs, _eWildcard = False}}
    _ -> return ()
identifiers _ = return ()