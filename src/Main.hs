module Main where

import System.Directory (getCurrentDirectory)
import System.Environment (getProgName)
import System.IO (hPutStrLn, stderr)
import System.FilePath ((</>), isAbsolute, takeDirectory)

import Cabal (findCabalFile)
import Client (getServerStatus, serverCommand, stopServer)
import CommandArgs
import Daemonize (daemonize)
import Server (startServer, createListenSocket)
import Types (Command(..))

absoluteFilePath :: FilePath -> IO FilePath
absoluteFilePath path = if isAbsolute path then return path else do
    dir <- getCurrentDirectory
    return $ dir </> path


defaultSocketPath :: IO FilePath
defaultSocketPath = do
    mbCabalFile <- getCurrentDirectory >>= findCabalFile
    case mbCabalFile of
        Nothing -> return socketFile
        Just cabalFile -> return $ takeDirectory cabalFile </> socketFile

  where socketFile :: FilePath
        socketFile = ".hdevtools.sock"


getSocketFilename :: Maybe FilePath -> IO FilePath
getSocketFilename Nothing = defaultSocketPath
getSocketFilename (Just f) = return f

main :: IO ()
main = do
    args <- loadHDevTools
    sock <- getSocketFilename (socket args)
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
                s <- createListenSocket sock
                daemonize True $ startServer sock (Just s)
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
    | otherwise = do
        absFile <- absoluteFilePath $ file args
        let args' = args { file = absFile }
        serverCommand sock (cmd args') (ghcOpts args')

doCheck :: FilePath -> HDevTools -> IO ()
doCheck = doFileCommand "check" $
    \args -> CmdCheck (file args)

doInfo :: FilePath -> HDevTools -> IO ()
doInfo = doFileCommand "info" $
    \args -> CmdInfo (file args) (identifier args)

doType :: FilePath -> HDevTools -> IO ()
doType = doFileCommand "type" $
    \args -> CmdType (file args) (line args, col args)
