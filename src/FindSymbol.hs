{-# Language ScopedTypeVariables #-}

module FindSymbol
    ( findSymbol
    ) where

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Control.Exception
import Data.List (find, nub)
import Data.Maybe (catMaybes, isJust)
import qualified GHC                    
import qualified UniqFM
import qualified Packages as PKG
import qualified Name
import Exception (ghandle)

type SymbolName = String
type ModuleName = String

findSymbol :: SymbolName -> [FilePath] -> GHC.Ghc [ModuleName]
findSymbol symbol files = do
   -- for the findsymbol command GHC shouldn't output any warnings
   -- or errors to stdout for the loaded source files, we're only
   -- interested in the module graph of the loaded targets
   dynFlags <- GHC.getSessionDynFlags
   _        <- GHC.setSessionDynFlags dynFlags { GHC.log_action = \_ _ _ _ _ -> return () }

   fileMods <- concat <$> mapM (findSymbolInFile symbol) files

   -- reset the old log_action
   _ <- GHC.setSessionDynFlags dynFlags

   pkgsMods <- findSymbolInPackages symbol
   return . nub . map (GHC.moduleNameString . GHC.moduleName) $ fileMods ++ pkgsMods


findSymbolInFile :: SymbolName -> FilePath -> GHC.Ghc [GHC.Module]
findSymbolInFile symbol file = do
   loadFile
   filterM (containsSymbol symbol) =<< fileModules
   where
   loadFile = do
      let noPhase = Nothing
      target <- GHC.guessTarget file noPhase
      GHC.setTargets [target]
      let handler err = GHC.printException err >> return GHC.Failed
      _ <- GHC.handleSourceError handler (GHC.load GHC.LoadAllTargets)
      return ()

   fileModules = map GHC.ms_mod <$> GHC.getModuleGraph


findSymbolInPackages :: SymbolName -> GHC.Ghc [GHC.Module]
findSymbolInPackages symbol =
   filterM (containsSymbol symbol) =<< allExposedModules
   where
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


containsSymbol :: SymbolName -> GHC.Module -> GHC.Ghc Bool
containsSymbol symbol module_ =
   isJust . find (== symbol) <$> allExportedSymbols
   where
   allExportedSymbols =
      ghandle (\(_ :: SomeException) -> return [])
              (do info <- GHC.getModuleInfo module_
                  return $ maybe [] (map Name.getOccString . GHC.modInfoExports) info)
