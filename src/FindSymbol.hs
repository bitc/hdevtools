
module FindSymbol
    ( findSymbol
    ) where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Exception
import Data.List (find)
import qualified GHC                    
import qualified UniqFM
import qualified Packages as PKG
import qualified Name
import Exception (ghandle)

findSymbol :: String -> GHC.Ghc [String]
findSymbol symbol = do
   modules <- allExposedModules
   modulesWith symbol modules
   where
   modulesWith sym = foldM (hasSym sym) []

   hasSym sym modsWithSym modul = do
      syms <- allExportedSymbols modul
      return $ case find (== sym) syms of
                   Just _ -> (GHC.moduleNameString modul) : modsWithSym
                   _      -> modsWithSym

allExportedSymbols :: GHC.ModuleName -> GHC.Ghc [String]
allExportedSymbols modul = do
   ghandle handleException $ do
      maybeInfo <- moduleInfo
      return $ case maybeInfo of
                  Just info -> exports info
                  _         -> []
   where
   handleException :: SomeException -> GHC.Ghc [String]
   handleException _ = return []

   exports    = map Name.getOccString . GHC.modInfoExports
   moduleInfo = GHC.findModule modul Nothing >>= GHC.getModuleInfo

allExposedModules :: GHC.Ghc [GHC.ModuleName]
allExposedModules = getExposedModules <$> GHC.getSessionDynFlags
   where
   getExposedModules = concatMap (\pkg -> if PKG.exposed pkg then PKG.exposedModules pkg else [])
                       . UniqFM.eltsUFM . PKG.pkgIdMap . GHC.pkgState
