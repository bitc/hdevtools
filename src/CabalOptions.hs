{-# LANGUAGE DeriveDataTypeable #-}
module CabalOptions where

import Data.Typeable (Typeable)
import Control.Exception (Exception, throwIO)
import Data.List (find)
import System.FilePath (takeExtension)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)
import System.Directory (getDirectoryContents)

findCabalFilePath :: IO (Maybe FilePath)
findCabalFilePath = do
    allFiles <- getDirectoryContents "."
    return $ find (\x -> takeExtension x == ".cabal") allFiles

data MissingCabalFileException = MissingCabalFileException
    deriving (Show, Typeable)

instance Exception MissingCabalFileException

test = do
    mbCabalFile <- findCabalFilePath
    cabalFile <- maybe (throwIO MissingCabalFileException) return mbCabalFile
    package <- readPackageDescription silent cabalFile
    print package
