{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module Info
    ( getIdentifierInfo
    , getType
    ) where

import Control.Monad (liftM)
import Data.Generics (GenericQ, mkQ, extQ, gmapQ)
import Data.List (find, sortBy, intersperse)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Typeable (Typeable)
import MonadUtils (liftIO)
import qualified CoreUtils
import qualified Desugar
#if __GLASGOW_HASKELL__ >= 706
import qualified DynFlags
#endif
#if __GLASGOW_HASKELL__ >= 708
import qualified HsExpr
#else
import qualified TcRnTypes
#endif
import qualified GHC
import qualified HscTypes
import qualified NameSet
import qualified Outputable
import qualified PprTyThing
import qualified Pretty
import qualified TcHsSyn

getIdentifierInfo :: FilePath -> String -> GHC.Ghc (Either String String)
getIdentifierInfo file identifier =
    withModSummary file $ \m -> do
#if __GLASGOW_HASKELL__ >= 706
        GHC.setContext [GHC.IIModule (GHC.moduleName (GHC.ms_mod m))]
#elif __GLASGOW_HASKELL__ >= 704
        GHC.setContext [GHC.IIModule (GHC.ms_mod m)]
#else
        GHC.setContext [GHC.ms_mod m] []
#endif
        GHC.handleSourceError (return . Left . show) $
            liftM Right (infoThing identifier)

getType :: FilePath -> (Int, Int) -> GHC.Ghc (Either String [((Int, Int, Int, Int), String)])
getType file (line, col) =
    withModSummary file $ \m -> do
        p <- GHC.parseModule m
        typechecked <- GHC.typecheckModule p
        types <- processTypeCheckedModule typechecked (line, col)
        return (Right types)

withModSummary :: String -> (HscTypes.ModSummary -> GHC.Ghc (Either String a)) -> GHC.Ghc (Either String a)
withModSummary file action = do
    let noPhase = Nothing
    target <- GHC.guessTarget file noPhase
    GHC.setTargets [target]

    let handler err = GHC.printException err >> return GHC.Failed
    flag <- GHC.handleSourceError handler (GHC.load GHC.LoadAllTargets)
    case flag of
        GHC.Failed -> return (Left "Error loading targets")
        GHC.Succeeded -> do
            modSummary <- getModuleSummary file
            case modSummary of
                Nothing -> return (Left "Module not found in module graph")
                Just m -> action m

getModuleSummary :: FilePath -> GHC.Ghc (Maybe GHC.ModSummary)
getModuleSummary file = do
    moduleGraph <- GHC.getModuleGraph
    case find (moduleSummaryMatchesFilePath file) moduleGraph of
        Nothing -> return Nothing
        Just moduleSummary -> return (Just moduleSummary)

moduleSummaryMatchesFilePath :: FilePath -> GHC.ModSummary -> Bool
moduleSummaryMatchesFilePath file moduleSummary =
    let location = GHC.ms_location moduleSummary
        location_file = GHC.ml_hs_file location
    in case location_file of
        Just f -> f == file
        Nothing -> False

------------------------------------------------------------------------------
-- Most of the following code was taken from the source code of 'ghc-mod' (with
-- some stylistic changes)
--
-- ghc-mod:
--     http://www.mew.org/~kazu/proj/ghc-mod/
--     https://github.com/kazu-yamamoto/ghc-mod/

processTypeCheckedModule :: GHC.TypecheckedModule -> (Int, Int) -> GHC.Ghc [((Int, Int, Int, Int), String)]
processTypeCheckedModule tcm (line, col) = do
    let tcs = GHC.tm_typechecked_source tcm
        bs = listifySpans tcs (line, col) :: [GHC.LHsBind GHC.Id]
        es = listifySpans tcs (line, col) :: [GHC.LHsExpr GHC.Id]
        ps = listifySpans tcs (line, col) :: [GHC.LPat GHC.Id]
    bts <- mapM (getTypeLHsBind tcm) bs
    ets <- mapM (getTypeLHsExpr tcm) es
    pts <- mapM (getTypeLPat tcm) ps
#if __GLASGOW_HASKELL__ >= 706
    dflags <- DynFlags.getDynFlags
    return $ map (toTup dflags) $
#else
    return $ map toTup $
#endif
        sortBy cmp $ catMaybes $ concat [ets, bts, pts]
    where
    cmp (a, _) (b, _)
        | a `GHC.isSubspanOf` b = LT
        | b `GHC.isSubspanOf` a = GT
        | otherwise = EQ

#if __GLASGOW_HASKELL__ >= 706
toTup :: GHC.DynFlags -> (GHC.SrcSpan, GHC.Type) -> ((Int, Int, Int, Int), String)
toTup dflags (spn, typ) = (fourInts spn, pretty dflags typ)
#else
toTup :: (GHC.SrcSpan, GHC.Type) -> ((Int, Int, Int, Int), String)
toTup (spn, typ) = (fourInts spn, pretty typ)
#endif

fourInts :: GHC.SrcSpan -> (Int, Int, Int, Int)
fourInts = fromMaybe (0, 0, 0, 0) . getSrcSpan

getSrcSpan :: GHC.SrcSpan -> Maybe (Int, Int, Int, Int)
getSrcSpan (GHC.RealSrcSpan spn) =
    Just (GHC.srcSpanStartLine spn
         , GHC.srcSpanStartCol spn
         , GHC.srcSpanEndLine spn
         , GHC.srcSpanEndCol spn)
getSrcSpan _ = Nothing

getTypeLHsBind :: GHC.TypecheckedModule -> GHC.LHsBind GHC.Id -> GHC.Ghc (Maybe (GHC.SrcSpan, GHC.Type))
#if __GLASGOW_HASKELL__ >= 708
getTypeLHsBind _ (GHC.L spn GHC.FunBind{GHC.fun_matches = grp}) = return $ Just (spn, HsExpr.mg_res_ty grp)
#else
getTypeLHsBind _ (GHC.L spn GHC.FunBind{GHC.fun_matches = GHC.MatchGroup _ typ}) = return $ Just (spn, typ)
#endif
getTypeLHsBind _ _ = return Nothing

getTypeLHsExpr :: GHC.TypecheckedModule -> GHC.LHsExpr GHC.Id -> GHC.Ghc (Maybe (GHC.SrcSpan, GHC.Type))
#if __GLASGOW_HASKELL__ >= 708
getTypeLHsExpr _ e = do
#else
getTypeLHsExpr tcm e = do
#endif
    hs_env <- GHC.getSession
#if __GLASGOW_HASKELL__ >= 708
    (_, mbe) <- liftIO $ Desugar.deSugarExpr hs_env e
#else
    let modu   = GHC.ms_mod $ GHC.pm_mod_summary $ GHC.tm_parsed_module tcm
        rn_env = TcRnTypes.tcg_rdr_env $ fst $ GHC.tm_internals_ tcm
        ty_env = TcRnTypes.tcg_type_env $ fst $ GHC.tm_internals_ tcm
    (_, mbe) <- liftIO $ Desugar.deSugarExpr hs_env modu rn_env ty_env e
#endif
    return ()
    case mbe of
        Nothing -> return Nothing
        Just expr -> return $ Just (GHC.getLoc e, CoreUtils.exprType expr)

getTypeLPat :: GHC.TypecheckedModule -> GHC.LPat GHC.Id -> GHC.Ghc (Maybe (GHC.SrcSpan, GHC.Type))
getTypeLPat _ (GHC.L spn pat) = return $ Just (spn, TcHsSyn.hsPatType pat)

listifySpans :: Typeable a => GHC.TypecheckedSource -> (Int, Int) -> [GHC.Located a]
listifySpans tcs lc = listifyStaged TypeChecker p tcs
    where
    p (GHC.L spn _) = GHC.isGoodSrcSpan spn && spn `GHC.spans` lc

listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))

#if __GLASGOW_HASKELL__ >= 706
pretty :: GHC.DynFlags -> GHC.Type -> String
pretty dflags =
#else
pretty :: GHC.Type -> String
pretty =
#endif
#if __GLASGOW_HASKELL__ >= 708
    Pretty.showDoc Pretty.OneLineMode 0
#else
    Pretty.showDocWith Pretty.OneLineMode
#endif
#if __GLASGOW_HASKELL__ >= 706
    . Outputable.withPprStyleDoc dflags
#else
    . Outputable.withPprStyleDoc
#endif
        (Outputable.mkUserStyle Outputable.neverQualify Outputable.AllTheWay)
#if __GLASGOW_HASKELL__ >= 708
    . PprTyThing.pprTypeForUser
#else
    . PprTyThing.pprTypeForUser False
#endif

------------------------------------------------------------------------------
-- The following was taken from 'ghc-syb-utils'
--
-- ghc-syb-utils:
--     https://github.com/nominolo/ghc-syb

-- | Ghc Ast types tend to have undefined holes, to be filled
--   by later compiler phases. We tag Asts with their source,
--   so that we can avoid such holes based on who generated the Asts.
data Stage = Parser | Renamer | TypeChecker deriving (Eq,Ord,Show)

-- | Like 'everything', but avoid known potholes, based on the 'Stage' that
--   generated the Ast.
everythingStaged :: Stage -> (r -> r -> r) -> r -> GenericQ r -> GenericQ r
everythingStaged stage k z f x
  | (const False `extQ` postTcType `extQ` fixity `extQ` nameSet) x = z
  | otherwise = foldl k (f x) (gmapQ (everythingStaged stage k z f) x)
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet.NameSet -> Bool
#if __GLASGOW_HASKELL__ >= 709
        postTcType = const (stage<TypeChecker)                 :: GHC.PostTc GHC.Id GHC.Type -> Bool
#else
        postTcType = const (stage<TypeChecker)                 :: GHC.PostTcType -> Bool
#endif
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool

------------------------------------------------------------------------------
-- The following code was taken from GHC's ghc/InteractiveUI.hs (with some
-- stylistic changes)

infoThing :: String -> GHC.Ghc String
infoThing str = do
    names <- GHC.parseName str
#if __GLASGOW_HASKELL__ >= 708
    mb_stuffs <- mapM (GHC.getInfo False) names
    let filtered = filterOutChildren (\(t,_f,_i,_) -> t) (catMaybes mb_stuffs)
#else
    mb_stuffs <- mapM GHC.getInfo names
    let filtered = filterOutChildren (\(t,_f,_i) -> t) (catMaybes mb_stuffs)
#endif
    unqual <- GHC.getPrintUnqual
#if __GLASGOW_HASKELL__ >= 706
    dflags <- DynFlags.getDynFlags
    return $ Outputable.showSDocForUser dflags unqual $
#else
    return $ Outputable.showSDocForUser unqual $
#endif
#if __GLASGOW_HASKELL__ >= 708
        Outputable.vcat (intersperse (Outputable.text "") $ map pprInfo filtered)
#else
        Outputable.vcat (intersperse (Outputable.text "") $ map (pprInfo False) filtered)
#endif

  -- Filter out names whose parent is also there Good
  -- example is '[]', which is both a type and data
  -- constructor in the same type
filterOutChildren :: (a -> HscTypes.TyThing) -> [a] -> [a]
filterOutChildren get_thing xs
  = filter (not . has_parent) xs
  where
    all_names = NameSet.mkNameSet (map (GHC.getName . get_thing) xs)
#if __GLASGOW_HASKELL__ >= 704
    has_parent x = case HscTypes.tyThingParent_maybe (get_thing x) of
#else
    has_parent x = case PprTyThing.pprTyThingParent_maybe (get_thing x) of
#endif
                     Just p  -> GHC.getName p `NameSet.elemNameSet` all_names
                     Nothing -> False

#if __GLASGOW_HASKELL__ >= 708
pprInfo :: (HscTypes.TyThing, GHC.Fixity, [GHC.ClsInst], [GHC.FamInst]) -> Outputable.SDoc
pprInfo (thing, fixity, insts, _) =
    PprTyThing.pprTyThingInContextLoc thing
#elif __GLASGOW_HASKELL__ >= 706
pprInfo :: PprTyThing.PrintExplicitForalls -> (HscTypes.TyThing, GHC.Fixity, [GHC.ClsInst]) -> Outputable.SDoc
pprInfo pefas (thing, fixity, insts) =
    PprTyThing.pprTyThingInContextLoc pefas thing
#else
pprInfo :: PprTyThing.PrintExplicitForalls -> (HscTypes.TyThing, GHC.Fixity, [GHC.Instance]) -> Outputable.SDoc
pprInfo pefas (thing, fixity, insts) =
    PprTyThing.pprTyThingInContextLoc pefas thing
#endif
        Outputable.$$ show_fixity fixity
        Outputable.$$ Outputable.vcat (map GHC.pprInstance insts)
    where
    show_fixity fix
        | fix == GHC.defaultFixity = Outputable.empty
        | otherwise                = Outputable.ppr fix Outputable.<+> Outputable.ppr (GHC.getName thing)
