module Client
    ( getServerStatus
    , stopServer
    , serverCommand
    ) where

import Network (PortID(UnixSocket), connectTo)
import System.Exit (exitFailure, exitWith)
import System.IO (Handle, hClose, hFlush, hGetLine, hPutStrLn, stderr)

import Types (ClientDirective(..), Command(..), ServerDirective(..))
import Util (readMaybe)

connect :: FilePath -> IO Handle
connect sock = do
    connectTo "" (UnixSocket sock)

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
    h <- connect sock
    hPutStrLn h $ show (SrvCommand cmd ghcOpts)
    hFlush h
    startClientReadLoop h

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
