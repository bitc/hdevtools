{-# LANGUAGE CPP #-}

module Client
    ( getServerStatus
    , stopServer
    , serverCommand
    ) where

import Control.Exception (tryJust)
import Control.Monad (guard)
import Network (connectTo)
#ifdef mingw32_HOST_OS
import Network (PortID(PortNumber))
#else
import Network (PortID(UnixSocket))
#endif
import System.Exit (exitFailure, exitWith)
import System.IO (Handle, hClose, hFlush, hGetLine, hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Directory (getCurrentDirectory)

import Daemonize (daemonize)
import Server (createListenSocket, startServer)
import Types (ClientDirective(..), Command(..), ServerDirective(..))
import Util (readMaybe, connect)

getServerStatus :: FilePath -> IO ()
getServerStatus sock = do
    h <- connect sock
    hPutStrLn h $ show SrvStatus
    hFlush h
    startClientReadLoop h

stopServer :: FilePath -> IO ()
stopServer sock = do
    h <- connect sock
    hPutStrLn h $ show SrvExit
    hFlush h
    startClientReadLoop h

serverCommand :: FilePath -> Command -> [String] -> IO ()
serverCommand sock cmd ghcOpts = do
    r <- tryJust (guard . isDoesNotExistError) (connect sock)
    case r of
        Right h -> do
            cwd <- getCurrentDirectory
            hPutStrLn h $ show (SrvCommand cwd cmd ghcOpts)
            hFlush h
            startClientReadLoop h
        Left _ -> do
            daemonize False sock
            serverCommand sock cmd ghcOpts

startClientReadLoop :: Handle -> IO ()
startClientReadLoop h = do
    msg <- hGetLine h
    let clientDirective = readMaybe msg
    case clientDirective of
        Just (ClientStdout out) -> putStrLn out >> startClientReadLoop h
        Just (ClientStderr err) -> hPutStrLn stderr err >> startClientReadLoop h
        Just (ClientExit exitCode) -> hClose h >> exitWith exitCode
        Just (ClientUnexpectedError err) -> hClose h >> unexpectedError err
        Nothing -> do
            hClose h
            unexpectedError $
                "The server sent an invalid message to the client: " ++ show msg

unexpectedError :: String -> IO ()
unexpectedError err = do
    hPutStrLn stderr banner
    hPutStrLn stderr err
    hPutStrLn stderr banner
    exitFailure
    where banner = replicate 78 '*'
