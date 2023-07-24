{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module GTD.Haskell.Parser.GhcLibParser where

import Control.Exception (try)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.State (MonadState (..), evalStateT, execStateT, modify)
import Control.Monad.Writer (MonadWriter (..), WriterT (..), execWriterT, mapWriterT)
import Data.Data (Data (..), showConstr)
import Data.Either (fromRight)
import qualified Data.Functor
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Errors.Types (DriverMessageOpts (psDiagnosticOpts))
import GHC.Driver.Ppr (showSDoc)
import GHC.Driver.Session (DynFlags (..), Language (..), PlatformMisc (..), Settings (..), defaultDynFlags, initDynFlags, parseDynamicFilePragma, supportedLanguagesAndExtensions)
import GHC.Fingerprint (fingerprint0)
import GHC.Hs (ConDecl (..), ConDeclField (..), DataDefnCons (..), FamilyDecl (..), FieldOcc (..), GhcPs, HsConDetails (..), HsDataDefn (..), HsDecl (..), HsModule (..), IE (..), IEWrappedName (..), ImportDecl (..), ImportDeclQualifiedStyle (..), ImportListInterpretation (..), LIdP, ModuleName (ModuleName), Sig (..), SrcSpanAnn' (..), TyClDecl (..))
import GHC.LanguageExtensions (Extension (..))
import GHC.Parser (parseModule)
import GHC.Parser.Errors.Types (PsMessage (..))
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer (P (unP), PState (..), ParseResult (..), ParserOpts, getPsMessages, initParserState, mkParserOpts)
import GHC.Platform (Arch (..), ArchOS (..), ByteOrder (..), OS (..), PlatformWordSize (..), genericPlatform)
import GHC.Settings (FileSettings (..), GhcNameVersion (..), Platform (..), ToolSettings (..), sTopDir)
import GHC.Settings.Config (cProjectVersion)
import GHC.Types.Error (DecoratedSDoc, Diagnostic (..), Messages (getMessages))
import GHC.Types.PkgQual (RawPkgQual (..))
import GHC.Types.SourceError (SourceError (SourceError), handleSourceError, srcErrorMessages)
import GHC.Types.SrcLoc (GenLocated (..), Located, RealSrcSpan (srcSpanFile), SrcSpan (..), mkRealSrcLoc, srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)
import GHC.Utils.Error (DiagOpts (..), pprMessages, pprMsgEnvelopeBagWithLocDefault)
import GHC.Utils.Outputable (Outputable (..), SDocContext (..), defaultSDocContext, renderWithContext)
import GHC.Utils.Panic (handleGhcException)
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Exports (..), ExportsOrImports (..), Imports (..), Module (..), SourceSpan (..), asDeclsMap, asExports, asImports, emptySourceSpan, name)
import GTD.Haskell.Parser.GhcLibParser.Extension (readExtension)
import GTD.Utils (logDebugNSS, modifyM)
import qualified Language.Haskell.Exts as HSE
import Language.Preprocessor.Cpphs (BoolOptions (lang))
import Text.Printf (printf)

showO :: Outputable a => a -> String
showO = renderWithContext defaultSDocContext {sdocErrorSpans = True} . ppr

showS :: Outputable a => SrcSpanAnn' a -> String
showS (SrcSpanAnn ann loc) = showO ann ++ " " ++ showO loc

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

parsePragmasIntoDynFlags :: DynFlags -> FilePath -> String -> IO (Maybe (DynFlags, [String]))
parsePragmasIntoDynFlags dynFlags p content = do
  let (_, opts) = getOptions (initParserOpts dynFlags) (stringToStringBuffer content) p
  -- TODO: warnings
  (flags, _, _) <- parseDynamicFilePragma dynFlags opts
  return $ Just (flags, (\(L _ n) -> n) <$> opts)

data HsModuleX = HsModuleX {_mod :: HsModule GhcPs, _languagePragmas :: [String]}

name :: HsModuleX -> String
name (HsModuleX HsModule {hsmodName = Just (L _ (ModuleName n))} _) = unpackFS n
name _ = ""

parse :: FilePath -> String -> IO (Either String HsModuleX)
parse p content = do
  let dynFlags0 = defaultDynFlags fakeSettings
  fM <- try (parsePragmasIntoDynFlags dynFlags0 p content) :: IO (Either SourceError (Maybe (DynFlags, [String])))
  let (dynFlags, languagePragmas) = fromMaybe (dynFlags0, []) $ fromRight Nothing fM

  printf "language pragmas: module %s -> %s\n" p (show languagePragmas)

  let opts = initParserOpts dynFlags
      location = mkRealSrcLoc (mkFastString p) 1 1
      buffer = stringToStringBuffer content
      parseState = initParserState opts buffer location
      r = unP parseModule parseState

  return $ case r of
    POk _ (L l e) -> Right $ HsModuleX e languagePragmas
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

declM :: String -> SrcSpan -> (Outputable a) => a -> Declaration
declM m loc k = Declaration {_declSrcOrig = asSourceSpan loc, _declModule = m, _declName = showO k}

declME :: String -> (Outputable a) => a -> Declaration
declME m k = Declaration {_declSrcOrig = emptySourceSpan, _declModule = m, _declName = showO k}

identifiers :: HsModuleX -> (MonadWriter Declarations m, MonadLoggerIO m) => m ()
identifiers (HsModuleX HsModule {hsmodName = Just (L _ (ModuleName nF)), hsmodDecls = decls} _) = do
  let mN = unpackFS nF
  let decl = declM mN
  let tellD loc k = tell mempty {_decls = asDeclsMap [decl loc k]}

  forM_ decls $ \(L _ d) -> case d of
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

asName :: IEWrappedName GhcPs -> (MonadLoggerIO m) => m (Maybe (LIdP GhcPs))
asName (IEName _ n) = return $ Just n
asName (IEType _ n) = return $ Just n
asName n = do
  logDebugNSS "asName" $ printf "not yet handled :t %s" (showConstr . toConstr $ n)
  return Nothing

importsOrExports :: String -> IE GhcPs -> (MonadWriter ExportsOrImports m, MonadLoggerIO m) => m ()
importsOrExports mN e = do
  let decl = declME mN
  case e of
    IEModuleContents _ (L _ mn) -> tell mempty {_eoiModules = [mempty {_modName = showO mn}]}
    IEVar _ (L _ n) -> do
      n1 <- asName n
      forM_ n1 $ \(L _ n2) -> tell mempty {_eoiDecls = [decl n2]}
    IEThingAbs _ (L _ n) -> do
      n1 <- asName n
      forM_ n1 $ \(L _ n2) ->
        tell mempty {_eoiCDs = [ClassOrData {_cdtName = decl n2, _cdtFields = mempty, _eWildcard = False}]}
    IEThingAll _ (L _ n) -> do
      n1 <- asName n
      forM_ n1 $ \(L _ n2) ->
        tell mempty {_eoiCDs = [ClassOrData {_cdtName = decl n2, _cdtFields = mempty, _eWildcard = True}]}
    IEThingWith _ (L _ n) wc ns -> do
      n1 <- asName n
      ns1 <- catMaybes <$> forM ns (\(L _ x) -> asName x)
      forM_ n1 $ \(L _ n2) ->
        tell mempty {_eoiCDs = [ClassOrData {_cdtName = decl n2, _cdtFields = asDeclsMap $ decl . (\(L _ x) -> x) <$> ns1, _eWildcard = False}]}
    _ -> return ()

exports0 :: HsModuleX -> (MonadWriter ExportsOrImports m, MonadLoggerIO m) => m Bool
exports0 m@(HsModuleX HsModule {hsmodName = Just (L _ (ModuleName nF)), hsmodExports = Just (L _ es)} _) = False <$ forM_ es (\(L _ e) -> importsOrExports (unpackFS nF) e)
exports0 (HsModuleX HsModule {hsmodExports = Nothing} _) = return True

exports :: HsModuleX -> (MonadWriter Exports m, MonadLoggerIO m) => m Bool
exports m = do
  (a, b) <- runWriterT $ exports0 m
  tell $ asExports b
  return a

imports0 :: HsModuleX -> (MonadWriter ExportsOrImports m, MonadLoggerIO m) => m ()
imports0 m@(HsModuleX HsModule {hsmodImports = is} _) = do
  forM_ is $ \(L _ (ImportDecl {ideclName = (L _ (ModuleName iMN)), ideclPkgQual = iPQ, ideclQualified = iQ, ideclAs = iA, ideclImportList = iIL})) -> do
    let iMNS = unpackFS iMN
    case iQ of
      QualifiedPre -> return ()
      QualifiedPost -> return ()
      NotQualified -> do
        case iPQ of
          RawPkgQual _ -> return ()
          NoRawPkgQual -> do
            case iA of
              Just _ -> return ()
              Nothing -> do
                case iIL of
                  Nothing -> tell mempty {_eoiModules = [mempty {_modName = iMNS}]}
                  Just (Exactly, L _ iis) -> do
                    forM_ iis $ \(L _ ii) -> importsOrExports iMNS ii
                  Just (EverythingBut, L _ iis) -> do
                    r <- execWriterT $ forM_ iis $ \(L _ ii) -> importsOrExports iMNS ii
                    tell mempty {_eoiModules = [mempty {_modName = iMNS, _hidingCDs = _eoiCDs r, _hidingDecls = _eoiDecls r}]}
                    return ()

imports :: HsModuleX -> (MonadWriter Imports m, MonadLoggerIO m) => m ()
imports m@(HsModuleX _ ps) = do
  (a, b) <- runWriterT $ imports0 m
  tell $ asImports b
  unless ("-XNoImplicitPrelude" `elem` ps) $
    tell mempty {importedModules = [mempty {_modName = "Prelude"}]}
  return a
