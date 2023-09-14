{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module GTD.Haskell.Parser.GhcLibParser where

import Control.Exception (try)
import Control.Monad (forM_, unless, (>=>))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.State (MonadIO (liftIO), MonadState (..), execStateT, modify)
import Control.Monad.Writer (MonadWriter (..))
import Data.Either (fromRight)
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Session (DynFlags (..), PlatformMisc (..), Settings (..), defaultDynFlags, parseDynamicFilePragma)
import GHC.Fingerprint (fingerprint0)
import GHC.Hs (ConDecl (..), ConDeclField (..), DataDefnCons (..), FamilyDecl (..), FieldOcc (..), GhcPs, HsConDetails (..), HsDataDefn (..), HsDecl (..), HsModule (..), IE (..), IEWrappedName (..), ImportDecl (..), ImportDeclQualifiedStyle (..), ImportListInterpretation (..), IsBootInterface (..), ModuleName (ModuleName), Sig (..), SrcSpanAnn' (..), TyClDecl (..), moduleNameString)
import GHC.Parser (parseHeader, parseIdentifier, parseModule)
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer (P (unP), PState (..), ParseResult (..), initParserState)
import GHC.Platform (genericPlatform)
import GHC.Settings (FileSettings (..), GhcNameVersion (..), ToolSettings (..))
import GHC.Settings.Config (cProjectVersion)
import GHC.Types.Name (HasOccName (..), occNameString)
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.SourceError (SourceError)
import GHC.Types.SrcLoc (GenLocated (..), RealSrcSpan (srcSpanFile), SrcSpan (..), mkRealSrcLoc, srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine, unLoc)
import qualified GHC.Unit.Types as GenModule
import GHC.Utils.Outputable (Outputable (..), SDocContext (..), defaultSDocContext, renderWithContext)
import GTD.Cabal.Types (ModuleNameS)
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Exports, Imports, Module (..), SourceSpan (..), asDeclsMap, emptySourceSpan)
import qualified GTD.Haskell.Declaration as Declarations
import GTD.Utils (logDebugNSS)
import Text.Printf (printf)

showO :: Outputable a => a -> String
showO = renderWithContext defaultSDocContext {sdocErrorSpans = True} . ppr

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

parse :: FilePath -> String -> (MonadLoggerIO m) => m (Either String HsModuleX)
parse p content = do
  let dynFlags0 = defaultDynFlags fakeSettings
  fM <- liftIO (try (parsePragmasIntoDynFlags dynFlags0 p content) :: IO (Either SourceError (Maybe (DynFlags, [String]))))
  let (dynFlags, languagePragmas) = fromMaybe (dynFlags0, []) $ fromRight Nothing fM

  logDebugNSS "GHC.parse" $ printf "language pragmas: module %s -> %s\n" p (show languagePragmas)

  let o = initParserOpts dynFlags
      l = mkRealSrcLoc (mkFastString p) 1 1
      b = stringToStringBuffer content
      s = initParserState o b l
      r = unP parseModule s

  case r of
    POk _ (L _ e) -> return $ Right $ HsModuleX e languagePragmas
    PFailed e -> do
      logDebugNSS "parse" $ printf "failed to parse %s via `parseModule`: %s" p (showO $ errors e)
      let s' = initParserState o b l
          r' = unP parseHeader s'
      case r' of
        POk _ (L _ e') -> return $ Right $ HsModuleX e' languagePragmas
        PFailed e2 -> do
          logDebugNSS "parse" $ printf "failed to parse %s via `parseHeader`: %s" p (showO $ errors e2)
          return $ Left $ showO $ errors e

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
asSourceSpan _ = emptySourceSpan

declM :: String -> SrcSpan -> (Outputable a) => a -> Declaration
declM m l k = Declaration {_declSrcOrig = asSourceSpan l, _declModule = m, _declName = showO k}

declME :: String -> (Outputable a) => a -> Declaration
declME m k = Declaration {_declSrcOrig = emptySourceSpan, _declModule = m, _declName = showO k}

declS :: String -> String -> Declaration
declS m k = Declaration {_declSrcOrig = emptySourceSpan, _declModule = m, _declName = k}

identifiers :: HsModuleX -> (MonadWriter Declarations m, MonadLoggerIO m) => m ()
identifiers (HsModuleX HsModule {hsmodName = Just (L (SrcSpanAnn _ _) (ModuleName nF)), hsmodDecls = decls} _) = do
  let mN = unpackFS nF
  let decl = declM mN
  let tellD l k = tell mempty {_decls = asDeclsMap [decl l k]}

  forM_ decls $ \(L _ d) -> case d of
    SigD _ s -> case s of
      TypeSig _ names _ -> do
        forM_ names $ \(L (SrcSpanAnn _ l) k) -> tellD l k
      _ -> return ()
    TyClD _ tc -> case tc of
      FamDecl {tcdFam = FamilyDecl {fdLName = (L (SrcSpanAnn _ l) k)}} -> tellD l k -- TODO: add more stuff?
      SynDecl {tcdLName = (L (SrcSpanAnn _ l) k)} -> tellD l k
      DataDecl {tcdLName = (L (SrcSpanAnn _ l) k), tcdDataDefn = (HsDataDefn _ _ _ _ ctorsD _)} -> do
        let ctors = case ctorsD of
              NewTypeCon a -> [a]
              DataTypeCons _ as -> as
        let fs = flip concatMap ctors $ \(L _ ctor) -> case ctor of
              ConDeclH98 {con_name = (L (SrcSpanAnn _ loc1) k1), con_args = ctor1} -> do
                let fields = case ctor1 of
                      PrefixCon _ _ -> []
                      InfixCon _ _ -> []
                      RecCon (L _ fs1) -> flip concatMap fs1 $ \(L _ (ConDeclField _ fs2 _ _)) ->
                        flip fmap fs2 $ \(L _ (FieldOcc _ (L (SrcSpanAnn _ loc2) k2))) ->
                          decl loc2 k2
                fields ++ [decl loc1 k1]
              _ -> []
        tell mempty {_dataTypes = Map.singleton (showO k) ClassOrData {_cdtName = decl l k, _cdtFields = asDeclsMap fs, _eWildcard = False}}
      ClassDecl {tcdLName = (L (SrcSpanAnn _ l) k), tcdSigs = ms} -> do
        let fs = flip concatMap ms $ \(L _ m) -> case m of
              TypeSig _ names _ -> flip fmap names $ \(L (SrcSpanAnn _ l) k) -> decl l k
              ClassOpSig _ _ names _ -> flip fmap names $ \(L (SrcSpanAnn _ l) k) -> decl l k
              _ -> []
        tell mempty {_dataTypes = Map.singleton (showO k) ClassOrData {_cdtName = decl l k, _cdtFields = asDeclsMap fs, _eWildcard = False}}
    _ -> return ()
identifiers _ = return ()

ieName :: IEWrappedName GhcPs -> Maybe RdrName
ieName (IEName _ n) = return $ unLoc n
ieName (IEType _ n) = return $ unLoc n
ieName _ = Nothing

-- logDebugNSS "ieName" $ printf "not yet handled :t %s" (showConstr . toConstr $ n)

rdr :: RdrName -> Maybe (String, String)
rdr (Unqual n) = return ("", occNameString n)
rdr (Exact n) = return ("", occNameString $ occName n)
rdr (Qual q n) = return (moduleNameString q, occNameString n)
rdr (Orig (GenModule.Module u q) n) = Nothing

-- logDebugNSS "asString@RdrName" $ printf "u=%s, mq=%s, n=%s" (show u) (show q) (show $ occNameString n)

ie :: String -> IE GhcPs -> (MonadState (Map.Map ModuleNameS Module) m) => m ()
ie _ (IEModuleContents _ (L _ n)) = modify $ Map.insertWith (<>) (moduleNameString n) mempty {_mName = moduleNameString n, _mType = Declarations.All}
ie mN e = do
  let decl = declS mN
      mn = case e of
        IEVar _ (L _ n) -> ieName n
        IEThingAbs _ (L _ n) -> ieName n
        IEThingAll _ (L _ n) -> ieName n
        IEThingWith _ (L _ n) _ _ -> ieName n
        _ -> Nothing
  case mn >>= rdr of
    Nothing -> return mempty
    Just (m, n) -> do
      z <- case e of
        IEVar _ _ -> return mempty {_mDecls = [decl n]}
        IEThingAbs _ _ -> return mempty {_mCDs = [ClassOrData {_cdtName = decl n, _cdtFields = mempty, _eWildcard = False}]}
        IEThingAll _ _ -> return mempty {_mCDs = [ClassOrData {_cdtName = decl n, _cdtFields = mempty, _eWildcard = True}]}
        IEThingWith _ _ _ ns -> do
          let ns1 = mapMaybe ((ieName >=> rdr) . unLoc) ns
          return mempty {_mCDs = [ClassOrData {_cdtName = decl n, _cdtFields = asDeclsMap $ decl . snd <$> ns1, _eWildcard = False}]}
        _ -> return mempty
      modify $ Map.insertWith (<>) m z {_mType = Declarations.Exactly}

exports :: HsModuleX -> (MonadState Exports m) => m Bool
exports (HsModuleX HsModule {hsmodExports = Just (L _ es), hsmodName = Just (L _ (ModuleName nF))} _) = do
  forM_ (unLoc <$> es) $ ie $ unpackFS nF
  return False
exports (HsModuleX HsModule {hsmodExports = Nothing} _) = return True

imports :: HsModuleX -> (MonadWriter Imports m) => m ()
imports (HsModuleX HsModule {hsmodImports = is} ps) = do
  forM_ is $ \(L _ (ImportDecl {ideclName = (L _ (ModuleName iMN)), ideclQualified = iQ, ideclSource = iS, ideclAs = iA, ideclImportList = iIL})) -> do
    case iS of
      IsBoot -> return ()
      NotBoot -> do
        let imn = unpackFS iMN
            mQ = maybe imn (\(L _ n) -> moduleNameString n) iA
            notQ = iQ == NotQualified

        let e = mempty {_mName = imn, _mQualifier = mQ, _mAllowNoQualifier = notQ}
        case iIL of
          Nothing -> tell [e {_mType = Declarations.All}]
          Just (x, L _ iis) -> do
            r <- fold <$> execStateT (forM_ (unLoc <$> iis) (ie imn)) Map.empty
            let eS = e {_mDecls = _mDecls r, _mCDs = _mCDs r}
            tell $ case x of
              Exactly -> [eS {_mType = Declarations.Exactly}]
              EverythingBut -> [eS {_mType = Declarations.EverythingBut}]

  unless ("-XNoImplicitPrelude" `elem` ps) $ do
    tell [mempty {_mName = "Prelude", _mQualifier = "Prelude"}]

---

identifier :: String -> Maybe (String, String)
identifier c = do
  let dynFlags0 = defaultDynFlags fakeSettings

  let opts = initParserOpts dynFlags0
      location = mkRealSrcLoc (mkFastString ".") 1 1
      buffer = stringToStringBuffer c
      parseState = initParserState opts buffer location
      r = unP parseIdentifier parseState

  case r of
    POk _ (L _ e) -> rdr e
    PFailed _ -> Nothing