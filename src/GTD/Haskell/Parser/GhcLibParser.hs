{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module GTD.Haskell.Parser.GhcLibParser where

import Control.Exception (try)
import Control.Monad (forM_, unless)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.State (MonadIO (liftIO), MonadState (..), execState, execStateT, modify)
import Control.Monad.Writer (MonadWriter (..))
import qualified Data.ByteString.Char8 as BSC8
import Data.Either (fromRight)
import Data.Foldable (Foldable (..))
import Data.Functor ((<&>))
import Data.Generics (everythingWithContext)
import qualified Data.HashMap.Strict as HMap
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Typeable (cast)
import GHC.Data.FastString (bytesFS, mkFastString, unpackFS)
import GHC.Data.StringBuffer (StringBuffer, stringBufferFromByteString, stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Session (DynFlags (..), PlatformMisc (..), Settings (..), defaultDynFlags, parseDynamicFilePragma)
import GHC.Fingerprint (fingerprint0)
import GHC.Hs (ConDecl (..), ConDeclField (..), DataDefnCons (..), FamilyDecl (..), FieldOcc (..), GhcPs, HsBindLR (..), HsConDetails (..), HsDataDefn (..), HsDecl (..), HsMatchContext (..), HsModule (..), IE (..), IEWrappedName (..), ImportDecl (..), ImportDeclQualifiedStyle (..), ImportListInterpretation (..), IsBootInterface (..), Match (..), MatchGroup (..), ModuleName (ModuleName), Sig (..), SrcSpanAnn' (..), SrcSpanAnnN, TyClDecl (..), moduleNameFS)
import GHC.Parser (parseHeader, parseIdentifier, parseModule)
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer (P (unP), PState (..), ParseResult (..), initParserState)
import GHC.Platform (genericPlatform)
import GHC.Settings (FileSettings (..), GhcNameVersion (..), ToolSettings (..))
import GHC.Settings.Config (cProjectVersion)
import GHC.Types.Name (HasOccName (..))
import GHC.Types.Name.Occurrence (OccName (..))
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.SourceError (SourceError)
import GHC.Types.SrcLoc (GenLocated (..), RealSrcSpan (srcSpanFile), SrcSpan (..), mkRealSrcLoc, srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine, unLoc)
import qualified GHC.Unit.Types as GenModule
import GHC.Utils.Outputable (Outputable (..), SDocContext (..), defaultSDocContext, renderWithContext)
import GTD.Cabal.Types (ModuleNameS)
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Exports, IdentifierWithUsageLocation (..), Imports, Module (..), SourceSpan (..), UsageType (..), asDeclsHMap, emptySourceSpan, srcSpans)
import qualified GTD.Haskell.Declaration as Declarations
import GTD.Utils (logDebugNSS)
import Text.Printf (printf)

showO :: (Outputable a) => a -> String
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

parsePragmasIntoDynFlags :: DynFlags -> FilePath -> StringBuffer -> IO (Maybe (DynFlags, [String]))
parsePragmasIntoDynFlags dynFlags p c = do
  let (_, opts) = getOptions (initParserOpts dynFlags) c p
  -- TODO: warnings
  (flags, _, _) <- parseDynamicFilePragma dynFlags opts
  return $ Just (flags, (\(L _ n) -> n) <$> opts)

data HsModuleX = HsModuleX {_mod :: HsModule GhcPs, _languagePragmas :: [String]}

name :: HsModuleX -> BSC8.ByteString
name (HsModuleX HsModule {hsmodName = Just (L _ (ModuleName n))} _) = bytesFS n
name _ = mempty

parse :: FilePath -> BSC8.ByteString -> (MonadLoggerIO m) => m (Either String HsModuleX)
parse p content = do
  let dynFlags0 = defaultDynFlags fakeSettings
      c = stringBufferFromByteString content
  fM <- liftIO (try (parsePragmasIntoDynFlags dynFlags0 p c) :: IO (Either SourceError (Maybe (DynFlags, [String]))))
  let (dynFlags, languagePragmas) = fromMaybe (dynFlags0, []) $ fromRight Nothing fM

  logDebugNSS "GHC.parse" $ printf "language pragmas: module %s -> %s\n" p (show languagePragmas)

  let o = initParserOpts dynFlags
      l = mkRealSrcLoc (mkFastString p) 1 1
      s = initParserState o c l
      r = unP parseModule s

  case r of
    POk _ (L _ e) -> return $ Right $ HsModuleX e languagePragmas
    PFailed e -> do
      logDebugNSS "parse" $ printf "failed to parse %s via `parseModule`: %s" p (showO $ errors e)
      let s' = initParserState o c l
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
    { _fileName = bytesFS $ srcSpanFile r,
      _lineBegin = srcSpanStartLine r,
      _colBegin = srcSpanStartCol r,
      _lineEnd = srcSpanEndLine r,
      _colEnd = srcSpanEndCol r
    }
asSourceSpan _ = emptySourceSpan

declM :: BSC8.ByteString -> SrcSpan -> RdrName -> Declaration
declM m l k = Declaration {_declSrcOrig = asSourceSpan l, _declSrcOthers = [], _declModule = m, _declName = cat . rdr $ k}

declME :: BSC8.ByteString -> RdrName -> Declaration
declME m k = Declaration {_declSrcOrig = emptySourceSpan, _declSrcOthers = [], _declModule = m, _declName = cat . rdr $ k}

declS :: BSC8.ByteString -> BSC8.ByteString -> Declaration
declS m k = Declaration {_declSrcOrig = emptySourceSpan, _declSrcOthers = [], _declModule = m, _declName = k}

identifiers :: HsModuleX -> (MonadWriter Declarations m, MonadLoggerIO m) => m ()
identifiers (HsModuleX HsModule {hsmodName = Just (L (SrcSpanAnn _ _) (ModuleName nF)), hsmodDecls = decls} _) = do
  let mN = bytesFS nF
  let decl = declM mN
      tellD l k = tell mempty {_decls = asDeclsHMap [decl l k]}
      tellC l k fs = tell mempty {_dataTypes = HMap.singleton (cat . rdr $ k) ClassOrData {_cdtName = decl l k, _cdtFields = fs, _eWildcard = False}}

  forM_ decls $ \(L _ d) -> case d of
    SigD _ s -> case s of
      TypeSig _ names _ ->
        forM_ names $ \(L (SrcSpanAnn _ l) k) -> tellD l k
      _ -> return ()
    ValD _ b -> case b of
      FunBind {fun_id = L (SrcSpanAnn _ l) k, fun_matches = MG {mg_alts = (L _ ks)}} ->
        forM_ ks $ \case
          L _ (Match {m_ctxt = FunRhs {mc_fun = L (SrcSpanAnn _ l1) k1}}) -> tellD l1 k1
          _ -> tellD l k
      _ -> return ()
    TyClD _ tc -> case tc of
      FamDecl {tcdFam = FamilyDecl {fdLName = (L (SrcSpanAnn _ l) k)}} -> tellC l k mempty
      SynDecl {tcdLName = (L (SrcSpanAnn _ l) k)} -> tellC l k mempty
      DataDecl {tcdLName = (L (SrcSpanAnn _ l) k), tcdDataDefn = (HsDataDefn {dd_cons = ctorsD})} -> do
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
        tellC l k (asDeclsHMap fs)
      ClassDecl {tcdLName = (L (SrcSpanAnn _ l) k), tcdSigs = ms} -> do
        let fs = flip concatMap ms $ \(L _ m) -> case m of
              TypeSig _ names _ -> flip fmap names $ \(L (SrcSpanAnn _ l) k) -> decl l k
              ClassOpSig _ _ names _ -> flip fmap names $ \(L (SrcSpanAnn _ l) k) -> decl l k
              _ -> []
        tellC l k (asDeclsHMap fs)
    _ -> return ()
identifiers _ = return ()

ieName :: IEWrappedName GhcPs -> Maybe RdrName
ieName (IEName _ n) = return $ unLoc n
ieName (IEType _ n) = return $ unLoc n
ieName _ = Nothing

-- logDebugNSS "ieName" $ printf "not yet handled :t %s" (showConstr . toConstr $ n)

moduleNameBS :: ModuleName -> BSC8.ByteString
moduleNameBS = bytesFS . moduleNameFS

rdr :: RdrName -> (BSC8.ByteString, BSC8.ByteString)
rdr (Unqual n) = (mempty, bytesFS $ occNameFS n)
rdr (Exact n) = (mempty, bytesFS $ occNameFS $ occName n)
rdr (Qual q n) = (moduleNameBS q, bytesFS $ occNameFS n)
rdr (Orig (GenModule.Module u q) n) = (moduleNameBS q, bytesFS $ occNameFS n)

cat :: (BSC8.ByteString, BSC8.ByteString) -> BSC8.ByteString
cat (m, n) | BSC8.null m = n
cat (m, n) = m <> BSC8.pack "." <> n

ie :: BSC8.ByteString -> IE GhcPs -> (MonadState (Map.Map ModuleNameS Module) m) => m ()
ie _ (IEModuleContents _ (L _ n)) = modify $ Map.insertWith (<>) (moduleNameBS n) mempty {_mName = moduleNameBS n, _mType = Declarations.All}
ie mN e = do
  let decl = declS mN
      mn = case e of
        IEVar _ (L _ n) -> ieName n
        IEThingAbs _ (L _ n) -> ieName n
        IEThingAll _ (L _ n) -> ieName n
        IEThingWith _ (L _ n) _ _ -> ieName n
        _ -> Nothing
  forM_ (rdr <$> mn) $ \(m, n) -> do
    z <- case e of
      IEVar _ _ -> return mempty {_mDecls = [decl n]}
      IEThingAbs _ _ -> return mempty {_mCDs = [ClassOrData {_cdtName = decl n, _cdtFields = mempty, _eWildcard = False}]}
      IEThingAll _ _ -> return mempty {_mCDs = [ClassOrData {_cdtName = decl n, _cdtFields = mempty, _eWildcard = True}]}
      IEThingWith _ _ _ ns -> do
        let ns1 = mapMaybe (fmap rdr . ieName . unLoc) ns
        return mempty {_mCDs = [ClassOrData {_cdtName = decl n, _cdtFields = asDeclsHMap $ decl . snd <$> ns1, _eWildcard = False}]}
      _ -> return mempty
    modify $ Map.insertWith (<>) m z {_mType = Declarations.Exactly}

exportsM :: HsModuleX -> (MonadState Exports m) => m ()
exportsM (HsModuleX HsModule {hsmodExports = Just (L _ es), hsmodName = Just (L _ (ModuleName nF))} _) =
  forM_ (unLoc <$> es) $ ie $ bytesFS nF
exportsM _ = return ()

exports :: HsModuleX -> Exports
exports m = execState (exportsM m) mempty

isImplicitExportAll :: HsModuleX -> Bool
isImplicitExportAll (HsModuleX HsModule {hsmodExports = Just (L _ es), hsmodName = Just (L _ (ModuleName nF))} _) = False
isImplicitExportAll (HsModuleX HsModule {hsmodExports = Nothing} _) = True
isImplicitExportAll _ = False

imports :: HsModuleX -> (MonadWriter Imports m) => m ()
imports (HsModuleX HsModule {hsmodImports = is} ps) = do
  forM_ is $ \(L _ (ImportDecl {ideclName = (L _ (ModuleName iMN)), ideclQualified = iQ, ideclSource = iS, ideclAs = iA, ideclImportList = iIL})) -> do
    case iS of
      IsBoot -> return ()
      NotBoot -> do
        let imn = bytesFS iMN
            mQ = maybe imn (\(L _ n) -> moduleNameBS n) iA
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

  unless ("-XNoImplicitPrelude" `elem` ps) $ tell [mempty {_mName = BSC8.pack "Prelude", _mQualifier = BSC8.pack "Prelude"}]

---

identifierUsages'raw :: HsModuleX -> [IdentifierWithUsageLocation]
identifierUsages'raw (HsModuleX m@HsModule {} _) = do
  let c :: HsModule GhcPs -> [(UsageType, GenLocated SrcSpanAnnN RdrName)]
      c = everythingWithContext Regular (<>) $ \a s -> do
        let c1 = (\(b :: GenLocated SrcSpanAnnN RdrName) -> ([(s, b)], s)) <$> cast a
        let c2 = (\(_ :: IE GhcPs) -> ([], if s == Regular then Export else s)) <$> cast a
        let c3 = (\(_ :: ImportDecl GhcPs) -> ([], if s == Regular then Import else s)) <$> cast a
        fromMaybe ([], s) $ listToMaybe $ catMaybes [c1, c2, c3]

  let ids = c m
  ids <&> \(ut, L (SrcSpanAnn _ l) n) ->
    let (mN, nN) = rdr n
     in IdentifierUsage {_iuName = nN, _iuModule = mN, _iuType = ut, _iuSourceSpan = asSourceSpan l}

identifierUsages'declarations :: Declarations -> [IdentifierWithUsageLocation] -> [IdentifierWithUsageLocation]
identifierUsages'declarations ds ius = do
  let ss = Set.fromList $ srcSpans ds
  let (l, r) = partition (\IdentifierUsage {_iuSourceSpan = s} -> s `Set.member` ss) ius
  r ++ fmap (\iu -> iu {_iuType = UDeclaration}) l

---

identifier :: BSC8.ByteString -> Maybe (SourceSpan, (BSC8.ByteString, BSC8.ByteString))
identifier c = do
  let dynFlags0 = defaultDynFlags fakeSettings

  let o = initParserOpts dynFlags0
      loc = mkRealSrcLoc (mkFastString ".") 1 1
      b = stringBufferFromByteString c
      st = initParserState o b loc
      r = unP parseIdentifier st

  case r of
    POk _ (L (SrcSpanAnn _ l) e) -> Just (asSourceSpan l, rdr e)
    PFailed _ -> Nothing