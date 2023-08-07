{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module GTD.Haskell.Parser.GhcLibParser where

import Control.Exception (try)
import Control.Monad (forM, forM_, guard, join, unless, void, when, (>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.State (MonadState (..), MonadTrans (..), evalStateT, execStateT, modify)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Writer (MonadWriter (..), WriterT (..), execWriterT, mapWriterT)
import Data.Data (Data (..), showConstr)
import Data.Either (fromRight)
import Data.Foldable (Foldable (..))
import qualified Data.Functor
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Errors.Types (DriverMessageOpts (psDiagnosticOpts))
import GHC.Driver.Ppr (showSDoc)
import GHC.Driver.Session (DynFlags (..), Language (..), PlatformMisc (..), Settings (..), defaultDynFlags, initDynFlags, parseDynamicFilePragma, supportedLanguagesAndExtensions)
import GHC.Fingerprint (fingerprint0)
import GHC.Hs (ConDecl (..), ConDeclField (..), DataDefnCons (..), FamilyDecl (..), FieldOcc (..), GhcPs, HsConDetails (..), HsDataDefn (..), HsDecl (..), HsModule (..), IE (..), IEWrappedName (..), ImportDecl (..), ImportDeclQualifiedStyle (..), ImportListInterpretation (..), IsBootInterface (..), LIdP, ModuleName (ModuleName), Sig (..), SrcSpanAnn' (..), TyClDecl (..), moduleNameString)
import GHC.LanguageExtensions (Extension (..))
import GHC.Parser (parseModule)
import GHC.Parser.Errors.Types (PsMessage (..))
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer (P (unP), PState (..), ParseResult (..), ParserOpts, getPsMessages, initParserState, mkParserOpts)
import GHC.Platform (Arch (..), ArchOS (..), ByteOrder (..), OS (..), PlatformWordSize (..), genericPlatform)
import GHC.Settings (FileSettings (..), GhcNameVersion (..), Platform (..), ToolSettings (..), sTopDir)
import GHC.Settings.Config (cProjectVersion)
import GHC.Types.Error (DecoratedSDoc, Diagnostic (..), Messages (getMessages))
import GHC.Types.Name (HasOccName (..), occNameString)
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.PkgQual (RawPkgQual (..))
import GHC.Types.SourceError (SourceError (SourceError), handleSourceError, srcErrorMessages)
import GHC.Types.SourceText (StringLiteral (..))
import GHC.Types.SrcLoc (GenLocated (..), Located, RealSrcSpan (srcSpanFile), SrcSpan (..), mkRealSrcLoc, srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine, unLoc)
import qualified GHC.Unit.Types as GenModule
import GHC.Utils.Error (DiagOpts (..), pprMessages, pprMsgEnvelopeBagWithLocDefault)
import GHC.Utils.Outputable (Outputable (..), SDocContext (..), defaultSDocContext, renderWithContext)
import GHC.Utils.Panic (handleGhcException)
import GTD.Cabal (ModuleNameS)
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Exports (..), ExportsOrImports (..), Imports (..), Module (..), SourceSpan (..), asDeclsMap, emptySourceSpan)
import qualified GTD.Haskell.Declaration as Declarations
import GTD.Resolution.Utils (ParallelizedState (_queue))
import GTD.Utils (logDebugNSS, logErrorNSS, modifyM)
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
    POk _ (L _ e) -> Right $ HsModuleX e languagePragmas
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

declS :: String -> String -> Declaration
declS m k = Declaration {_declSrcOrig = emptySourceSpan, _declModule = m, _declName = k}

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

ieName :: IEWrappedName GhcPs -> (MonadLoggerIO m) => MaybeT m RdrName
ieName (IEName _ n) = return $ unLoc n
ieName (IEType _ n) = return $ unLoc n
ieName n = do
  logDebugNSS "ieName" $ printf "not yet handled :t %s" (showConstr . toConstr $ n)
  MaybeT $ pure Nothing

rdr :: RdrName -> (MonadLoggerIO m) => MaybeT m (String, String)
rdr (Unqual n) = return ("", occNameString n)
rdr (Exact n) = return ("", occNameString $ occName n)
rdr (Qual q n) = return (moduleNameString q, occNameString n)
rdr (Orig (GenModule.Module u q) n) = do
  logDebugNSS "asString@RdrName" $ printf "u=%s, mq=%s, n=%s" (show u) (show q) (show $ occNameString n)
  MaybeT $ pure Nothing

ie :: String -> IE GhcPs -> (MonadState (Map.Map ModuleNameS Module) m, MonadLoggerIO m) => m ()
ie mN (IEModuleContents _ (L _ n)) = modify $ Map.insertWith (<>) (moduleNameString n) mempty {_mName = moduleNameString n, _mType = Declarations.All}
ie mN e = do
  let decl = declS mN
  void $ runMaybeT $ do
    (m, n) <- case e of
      IEVar _ (L _ n) -> ieName n >>= rdr
      IEThingAbs _ (L _ n) -> ieName n >>= rdr
      IEThingAll _ (L _ n) -> ieName n >>= rdr
      IEThingWith _ (L _ n) wc ns -> ieName n >>= rdr
      _ -> MaybeT $ pure Nothing
    z <- case e of
      IEVar _ _ -> return mempty {_mDecls = [decl n]}
      IEThingAbs _ _ -> return mempty {_mCDs = [ClassOrData {_cdtName = decl n, _cdtFields = mempty, _eWildcard = False}]}
      IEThingAll _ _ -> return mempty {_mCDs = [ClassOrData {_cdtName = decl n, _cdtFields = mempty, _eWildcard = True}]}
      IEThingWith _ _ wc ns -> do
        ns1 <- forM (unLoc <$> ns) (ieName >=> rdr)
        return mempty {_mCDs = [ClassOrData {_cdtName = decl n, _cdtFields = asDeclsMap $ decl . snd <$> ns1, _eWildcard = False}]}
    modify $ Map.insertWith (<>) m z {_mType = Declarations.Exactly}

exports :: HsModuleX -> (MonadState Exports m, MonadLoggerIO m) => m Bool
exports (HsModuleX HsModule {hsmodName = Just (L _ (ModuleName nF)), hsmodExports = Just (L _ es)} _) = do
  forM_ (unLoc <$> es) $ ie $ unpackFS nF
  return False
exports (HsModuleX HsModule {hsmodExports = Nothing} _) = return True

imports :: HsModuleX -> (MonadWriter Imports m, MonadLoggerIO m) => m ()
imports m@(HsModuleX HsModule {hsmodImports = is} ps) = do
  let logTag = "imports0 " ++ name m
  forM_ is $ \(L _ (ImportDecl {ideclName = (L _ (ModuleName iMN)), ideclQualified = iQ, ideclSource = iS, ideclAs = iA, ideclImportList = iIL})) -> do
    case iS of
      IsBoot -> return ()
      NotBoot -> do
        let imn = unpackFS iMN
            mQ = maybe imn (\(L _ n) -> moduleNameString n) iA
            allowNoQ = iQ == NotQualified

        case iIL of
          Nothing -> tell $ Map.singleton imn mempty {_mName = imn, _mQualifier = mQ, _mAllowNoQualifier = allowNoQ, _mType = Declarations.All}
          Just (x, L _ iis) -> do
            r :: Module <- fold <$> execStateT (forM_ (unLoc <$> iis) (ie imn)) Map.empty
            tell $ case x of
              Exactly -> Map.singleton imn mempty {_mName = imn, _mQualifier = mQ, _mAllowNoQualifier = allowNoQ, _mType = Declarations.Exactly, _mDecls = _mDecls r, _mCDs = _mCDs r}
              EverythingBut -> Map.singleton imn mempty {_mName = imn, _mQualifier = mQ, _mAllowNoQualifier = allowNoQ, _mType = Declarations.EverythingBut, _mDecls = _mDecls r, _mCDs = _mCDs r}

  unless ("-XNoImplicitPrelude" `elem` ps) $ do
    tell $ Map.singleton "Prelude" mempty {_mName = "Prelude", _mQualifier = "Prelude"}
