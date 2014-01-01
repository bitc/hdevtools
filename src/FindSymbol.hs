{-# Language ScopedTypeVariables #-}

module FindSymbol
    ( findSymbol
    ) where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Exception
import Data.List (find)
import Data.Maybe (catMaybes)
import qualified GHC                    
import qualified UniqFM
import qualified Packages as PKG
import qualified Name
import Exception (ghandle)

findSymbol :: String -> GHC.Ghc [String]
findSymbol symbol = do
   graphModules <- modulesWith symbol =<< allModulesFromGraph
   expModules   <- modulesWith symbol =<< allExposedModules
   return $ graphModules ++ expModules
   where
   modulesWith sym = foldM (hasSym sym) []

   hasSym sym modsWithSym modul = do
      syms <- allExportedSymbols modul
      return $ case find (== sym) syms of
                   Just _ -> (GHC.moduleNameString . GHC.moduleName $ modul) : modsWithSym
                   _      -> modsWithSym

allExportedSymbols :: GHC.Module -> GHC.Ghc [String]
allExportedSymbols module_ =
   ghandle (\(_ :: SomeException) -> return [])
           (do info <- GHC.getModuleInfo module_
               return $ maybe [] (map Name.getOccString . GHC.modInfoExports) info)

allModulesFromGraph :: GHC.Ghc [GHC.Module]
allModulesFromGraph = do
    moduleGraph <- GHC.getModuleGraph
    return $ map GHC.ms_mod moduleGraph

allExposedModules :: GHC.Ghc [GHC.Module]
allExposedModules = do
   modNames <- exposedModuleNames <$> GHC.getSessionDynFlags
   catMaybes <$> mapM findModule modNames
   where
   exposedModuleNames = concatMap (\pkg -> if PKG.exposed pkg then PKG.exposedModules pkg else [])
                        . UniqFM.eltsUFM . PKG.pkgIdMap . GHC.pkgState

findModule :: GHC.ModuleName -> GHC.Ghc (Maybe GHC.Module)
findModule moduleName =
   ghandle (\(_ :: SomeException) -> return Nothing)
           (Just <$> GHC.findModule moduleName Nothing)
