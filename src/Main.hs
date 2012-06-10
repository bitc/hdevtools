module Main where

import System.Environment (getProgName)
import System.IO (hPutStrLn, stderr)

import Client (getServerStatus, serverCommand, stopServer)
import CommandArgs
import Daemonize (daemonize)
import Server (startServer, createListenSocket)
import Types (Command(..))

defaultSocketFilename :: FilePath
defaultSocketFilename = ".hdevtools.sock"

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

doCheck :: FilePath -> HDevTools -> IO ()
doCheck sock args
    | null (file args) = do
        progName <- getProgName
        hPutStrLn stderr "You must provide a haskell source file. See:"
        hPutStrLn stderr $ progName ++ " check --help"
    | otherwise = do
        serverCommand sock (CmdCheck (file args)) (ghcOpts args)
