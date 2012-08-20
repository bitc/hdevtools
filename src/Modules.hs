module Modules
    ( getModules
    ) where

import Control.Monad (filterM)
import qualified GHC
import qualified Packages
import qualified UniqFM
import qualified Outputable
import Distribution.Package (PackageName(PackageName), pkgName)

getModules :: GHC.Ghc [(String, String)]
getModules = do
    dflags <- GHC.getSessionDynFlags
    let pkg_mods = allExposedModules dflags
    --loaded_mods <- liftM (map GHC.ms_mod_name) getLoadedModules
    return $ map formatMod (pkg_mods)
    where
    formatMod :: (GHC.ModuleName, String) -> (String, String)
    formatMod (modName, pkgId) = (Outputable.showPpr modName, pkgId)

allExposedModules :: GHC.DynFlags -> [(GHC.ModuleName, String)]
allExposedModules dflags =
    let packages = (filter Packages.exposed (UniqFM.eltsUFM pkg_db))
    in concatMap pkg_modules packages
    where
    pkg_db = Packages.pkgIdMap (GHC.pkgState dflags)
    pkg_modules :: Packages.InstalledPackageInfo_ GHC.ModuleName -> [(GHC.ModuleName, String)]
    pkg_modules pkg =
        let modules = Packages.exposedModules pkg
            PackageName pkg_id = pkgName (Packages.sourcePackageId pkg)
        in zip modules (repeat pkg_id)


getLoadedModules :: GHC.Ghc [GHC.ModSummary]
getLoadedModules = do
    graph <- GHC.getModuleGraph
    filterM (GHC.isLoaded . GHC.ms_mod_name) graph
