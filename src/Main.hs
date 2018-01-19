module Main where

import Data.Maybe (fromMaybe)
import Data.Traversable (Traversable(..))
import System.Directory (getCurrentDirectory)
import System.Environment (getProgName)
import System.IO (hPutStrLn, stderr)
import System.FilePath ((</>), isAbsolute, takeDirectory)

import Cabal (findCabalFile)
import Client (getServerStatus, serverCommand, stopServer)
import CommandArgs
import Daemonize (daemonize)
import Server (startServer, createListenSocket)
import Types (Command(..), CommandExtra(..), emptyCommandExtra)

absoluteFilePath :: FilePath -> IO FilePath
absoluteFilePath path = if isAbsolute path then return path else do
    dir <- getCurrentDirectory
    return $ dir </> path


defaultSocketFile :: FilePath
defaultSocketFile = ".hdevtools.sock"


fileArg :: HDevTools -> Maybe String
fileArg (Admin {})      = Nothing
fileArg (ModuleFile {}) = Nothing
fileArg args@(Check {}) = Just $ file args
fileArg args@(Info  {}) = Just $ file args
fileArg args@(Type  {}) = Just $ file args


main :: IO ()
main = do
    args <- loadHDevTools
    dir  <- maybe getCurrentDirectory (return . takeDirectory) $ fileArg args
    mCabalFile <- findCabalFile dir >>= traverse absoluteFilePath
    let extra = emptyCommandExtra
                    { ceGhcOptions = ghcOpts args
                    , ceCabalConfig = mCabalFile
                    , ceCabalOptions = cabalOpts args
                    }

    let defaultSocketPath = maybe "" takeDirectory mCabalFile </> defaultSocketFile
    let sock = fromMaybe defaultSocketPath $ socket args

    case args of
        Admin {} -> doAdmin sock args extra
        Check {} -> doCheck sock args extra
        ModuleFile {} -> doModuleFile sock args extra
        Info {} -> doInfo sock args extra
        Type {} -> doType sock args extra

doAdmin :: FilePath -> HDevTools -> CommandExtra -> IO ()
doAdmin sock args _extra
    | start_server args =
        if noDaemon args then startServer sock Nothing
            else do
                s <- createListenSocket sock
                daemonize True $ startServer sock (Just s)
    | status args = getServerStatus sock
    | stop_server args = stopServer sock
    | otherwise = do
        progName <- getProgName
        hPutStrLn stderr "You must provide a command. See:"
        hPutStrLn stderr $ progName ++ " --help"

doModuleFile :: FilePath -> HDevTools -> CommandExtra -> IO ()
doModuleFile sock args extra =
    serverCommand sock (CmdModuleFile (module_ args)) extra

doFileCommand :: String -> (HDevTools -> Command) -> FilePath -> HDevTools -> CommandExtra -> IO ()
doFileCommand cmdName cmd sock args extra
    | null (file args) = do
        progName <- getProgName
        hPutStrLn stderr "You must provide a haskell source file. See:"
        hPutStrLn stderr $ progName ++ " " ++ cmdName ++ " --help"
    | otherwise = do
        absFile <- absoluteFilePath $ file args
        let args' = args { file = absFile }
        serverCommand sock (cmd args') extra

doCheck :: FilePath -> HDevTools -> CommandExtra -> IO ()
doCheck = doFileCommand "check" $
    \args -> CmdCheck (file args)

doInfo :: FilePath -> HDevTools -> CommandExtra -> IO ()
doInfo = doFileCommand "info" $
    \args -> CmdInfo (file args) (identifier args)

doType :: FilePath -> HDevTools -> CommandExtra -> IO ()
doType = doFileCommand "type" $
    \args -> CmdType (file args) (line args, col args)
