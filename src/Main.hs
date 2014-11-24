{-# LANGUAGE CPP #-}

module Main where

import System.Environment (getProgName, getExecutablePath)
import System.IO (hPutStrLn, stderr)

import Client (getServerStatus, serverCommand, stopServer)
import CommandArgs
import Server (startServer, createListenSocket)
import Daemonize (daemonize)
import Types (Command(..))

defaultSocketFilename :: FilePath
#ifdef mingw32_HOST_OS
defaultSocketFilename = show 43210
#else
defaultSocketFilename = ".hdevtools.sock"
#endif

getSocketFilename :: Maybe FilePath -> FilePath
getSocketFilename Nothing = defaultSocketFilename
getSocketFilename (Just f) = f

main :: IO ()
main = do
    args <- loadHDevTools
    let sock = getSocketFilename (socket args)
    case args of
        Admin {} -> doAdmin sock args
        Check {} -> doCheck sock args
        ModuleFile {} -> doModuleFile sock args
        Info {} -> doInfo sock args
        Type {} -> doType sock args

doAdmin :: FilePath -> HDevTools -> IO ()
doAdmin sock args
    | start_server args =
        if noDaemon args then startServer sock Nothing
            else do
                daemonize True sock
    | status args = getServerStatus sock
    | stop_server args = stopServer sock
    | otherwise = do
        progName <- getProgName
        hPutStrLn stderr "You must provide a command. See:"
        hPutStrLn stderr $ progName ++ " --help"

doModuleFile :: FilePath -> HDevTools -> IO ()
doModuleFile sock args =
    serverCommand sock (CmdModuleFile (module_ args)) (ghcOpts args)

doFileCommand :: String -> (HDevTools -> Command) -> FilePath -> HDevTools -> IO ()
doFileCommand cmdName cmd sock args
    | null (file args) = do
        progName <- getProgName
        hPutStrLn stderr "You must provide a haskell source file. See:"
        hPutStrLn stderr $ progName ++ " " ++ cmdName ++ " --help"
    | otherwise = serverCommand sock (cmd args) (ghcOpts args)

doCheck :: FilePath -> HDevTools -> IO ()
doCheck = doFileCommand "check" $
    \args -> CmdCheck (file args)

doInfo :: FilePath -> HDevTools -> IO ()
doInfo = doFileCommand "info" $
    \args -> CmdInfo (file args) (identifier args)

doType :: FilePath -> HDevTools -> IO ()
doType = doFileCommand "type" $
    \args -> CmdType (file args) (line args, col args)
