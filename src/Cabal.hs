{-# LANGUAGE CPP #-}
module Cabal
  ( getPackageGhcOpts
  , findCabalFile
  ) where

#ifdef ENABLE_CABAL

import Control.Exception (IOException, catch)
import Data.Char (isSpace)
import Data.List (foldl', nub, sort, find, isPrefixOf, isSuffixOf)
import Data.Monoid (Monoid(..))
import Distribution.Package (PackageIdentifier(..), PackageName)
import Distribution.PackageDescription (PackageDescription(..), Executable(..), TestSuite(..), Benchmark(..), emptyHookedBuildInfo, buildable, libBuildInfo)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Configure (configure)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..), ComponentLocalBuildInfo(..),
    Component(..), ComponentName(..),
#if __GLASGOW_HASKELL__ < 707
    allComponentsBy,
#endif
    componentBuildInfo, foldComponent)
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Simple.GHC (componentGhcOptions)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Program.Db (lookupProgram)
import Distribution.Simple.Program.Types (ConfiguredProgram(programVersion), simpleProgram)
import Distribution.Simple.Program.GHC (GhcOptions(..), renderGhcOptions)
import Distribution.Simple.Setup (ConfigFlags(..), defaultConfigFlags, toFlag)
import Distribution.Verbosity (silent)
import Distribution.Version (Version(..))

import System.IO.Error (ioeGetErrorString)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath (takeDirectory, splitFileName, (</>))

componentName :: Component -> ComponentName
componentName =
    foldComponent (const CLibName)
                  (CExeName . exeName)
                  (CTestName . testName)
                  (CBenchName . benchmarkName)

getComponentLocalBuildInfo :: LocalBuildInfo -> ComponentName -> ComponentLocalBuildInfo
#if __GLASGOW_HASKELL__ >= 707
getComponentLocalBuildInfo lbi cname = getLocalBuildInfo cname $ componentsConfigs lbi
    where getLocalBuildInfo cname' ((cname'', clbi, _):cfgs) =
            if cname' == cname'' then clbi else getLocalBuildInfo cname' cfgs
          getLocalBuildInfo _ [] = error $ "internal error: missing config"
#else
getComponentLocalBuildInfo lbi CLibName =
    case libraryConfig lbi of
        Nothing -> error $ "internal error: missing library config"
        Just clbi -> clbi
getComponentLocalBuildInfo lbi (CExeName name) =
    case lookup name (executableConfigs lbi) of
        Nothing -> error $ "internal error: missing config for executable " ++ name
        Just clbi -> clbi
getComponentLocalBuildInfo lbi (CTestName name) =
    case lookup name (testSuiteConfigs lbi) of
        Nothing -> error $ "internal error: missing config for test suite " ++ name
        Just clbi -> clbi
getComponentLocalBuildInfo lbi (CBenchName name) =
    case lookup name (testSuiteConfigs lbi) of
        Nothing -> error $ "internal error: missing config for benchmark " ++ name
        Just clbi -> clbi
#endif

#if __GLASGOW_HASKELL__ >= 707
-- TODO: Fix callsites so we don't need `allComponentsBy`. It was taken from
-- http://hackage.haskell.org/package/Cabal-1.16.0.3/docs/src/Distribution-Simple-LocalBuildInfo.html#allComponentsBy
-- since it doesn't exist in Cabal 1.18.*
--
-- | Obtains all components (libs, exes, or test suites), transformed by the
-- given function.  Useful for gathering dependencies with component context.
allComponentsBy :: PackageDescription
                -> (Component -> a)
                -> [a]
allComponentsBy pkg_descr f =
    [ f (CLib  lib) | Just lib <- [library pkg_descr]
                    , buildable (libBuildInfo lib) ]
 ++ [ f (CExe  exe) | exe <- executables pkg_descr
                    , buildable (buildInfo exe) ]
 ++ [ f (CTest tst) | tst <- testSuites pkg_descr
                    , buildable (testBuildInfo tst)
                    , testEnabled tst ]
 ++ [ f (CBench bm) | bm <- benchmarks pkg_descr
                    , buildable (benchmarkBuildInfo bm)
                    , benchmarkEnabled bm ]
#endif


getPackageGhcOpts :: FilePath -> IO (Either String [String])
getPackageGhcOpts path = do
    getPackageGhcOpts' `catch` (\e -> do
        return $ Left $ "Cabal error: " ++ (ioeGetErrorString (e :: IOException)))
  where
    getPackageGhcOpts' :: IO (Either String [String])
    getPackageGhcOpts' = do
        genPkgDescr <- readPackageDescription silent path

        let cfgFlags' = (defaultConfigFlags defaultProgramConfiguration)
                            { configDistPref = toFlag $ takeDirectory path </> "dist"
                            -- TODO: figure out how to find out this flag
                            , configUserInstall = toFlag True
                            }

        let sandboxConfig = takeDirectory path </> "cabal.sandbox.config"
        exists <- doesFileExist sandboxConfig

        cfgFlags <- case exists of
                         False -> return cfgFlags'
                         True -> do
                             sandboxPackageDb <- getSandboxPackageDB sandboxConfig
                             return $ cfgFlags'
                                          { configPackageDBs = [Just sandboxPackageDb]
                                          }

        localBuildInfo <- configure (genPkgDescr, emptyHookedBuildInfo) cfgFlags

        let pkgDescr = localPkgDescr localBuildInfo
        let baseDir = fst . splitFileName $ path
        case getGhcVersion localBuildInfo of
            Nothing -> return $ Left "GHC is not configured"
            Just ghcVersion -> do
                let mbLibName = pkgLibName pkgDescr

                let ghcOpts' = foldl' mappend mempty $ map (getComponentGhcOptions localBuildInfo) $ flip allComponentsBy (\c -> c) . localPkgDescr $ localBuildInfo
                    -- FIX bug in GhcOptions' `mappend`
                    ghcOpts = ghcOpts' { ghcOptExtra = filter (/= "-Werror") $ nub $ ghcOptExtra ghcOpts'
                                       , ghcOptPackageDBs = sort $ nub (ghcOptPackageDBs ghcOpts')
                                       , ghcOptPackages = filter (\(_, pkgId) -> Just (pkgName pkgId) /= mbLibName) $ nub (ghcOptPackages ghcOpts')
                                       , ghcOptSourcePath = map (baseDir </>) (ghcOptSourcePath ghcOpts')
                                       }

                return $ Right $ renderGhcOptions ghcVersion ghcOpts

    pkgLibName :: PackageDescription -> Maybe PackageName
    pkgLibName pkgDescr = if hasLibrary pkgDescr
                          then Just $ pkgName . package $ pkgDescr
                          else Nothing

    hasLibrary :: PackageDescription -> Bool
    hasLibrary = maybe False (\_ -> True) . library

    getComponentGhcOptions :: LocalBuildInfo -> Component -> GhcOptions
    getComponentGhcOptions lbi comp =
        componentGhcOptions silent lbi bi clbi (buildDir lbi)

      where bi   = componentBuildInfo comp
            clbi = getComponentLocalBuildInfo lbi (componentName comp)

    getGhcVersion :: LocalBuildInfo -> Maybe Version
    getGhcVersion lbi = let db = withPrograms lbi
                         in do ghc <- lookupProgram (simpleProgram "ghc") db
                               programVersion ghc

    getSandboxPackageDB :: FilePath -> IO PackageDB
    getSandboxPackageDB sandboxPath = do
        contents <- readFile sandboxPath
        return $ SpecificPackageDB $ extractValue . parse $ contents
      where
        pkgDbKey = "package-db:"
        parse = head . filter (pkgDbKey `isPrefixOf`) . lines
        extractValue = fst . break isSpace . dropWhile isSpace . drop (length pkgDbKey)


findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile dir = do
    allFiles <- getDirectoryContents dir
    let mbCabalFile = find (isCabalFile) allFiles
    case mbCabalFile of
      Just cabalFile -> return $ Just $ dir </> cabalFile
      Nothing ->
        let parentDir = takeDirectory dir
         in if parentDir == dir
            then return Nothing
            else findCabalFile parentDir

  where

    isCabalFile :: FilePath -> Bool
    isCabalFile path = cabalExtension `isSuffixOf` path
                    && length path > length cabalExtension
        where cabalExtension = ".cabal"

# else

getPackageGhcOpts :: FilePath -> IO (Either String [String])
getPackageGhcOpts _ = return $ Right []

findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile _ = return Nothing

#endif
