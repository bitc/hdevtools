module Server where

import Control.Exception (bracket, finally, tryJust)
import Control.Monad (guard)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network (PortID(UnixSocket), Socket, accept, listenOn, sClose)
import System.Directory (removeFile)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (Handle, hClose, hFlush, hGetLine, hPutStrLn)
import System.IO.Error (isDoesNotExistError)

import CommandLoop (startCommandLoop)
import Types (ClientDirective(..), Command, ServerDirective(..))
import Util (readMaybe)

createListenSocket :: FilePath -> IO Socket
createListenSocket socketPath =
    listenOn (UnixSocket socketPath)

startServer :: FilePath -> Maybe Socket -> IO ()
startServer socketPath mbSock = do
    case mbSock of
        Nothing -> bracket (createListenSocket socketPath) cleanup go
        Just sock -> (go sock) `finally` (cleanup sock)
    where
    cleanup :: Socket -> IO ()
    cleanup sock = do
        sClose sock
        removeSocketFile

    go :: Socket -> IO ()
    go sock = do
        currentClient <- newIORef Nothing
        startCommandLoop (clientSend currentClient) (getNextCommand currentClient sock) [] Nothing

    removeSocketFile :: IO ()
    removeSocketFile = do
        -- Ignore possible error if socket file does not exist
        _ <- tryJust (guard . isDoesNotExistError) $ removeFile socketPath
        return ()

clientSend :: IORef (Maybe Handle) -> ClientDirective -> IO ()
clientSend currentClient clientDirective = do
    mbH <- readIORef currentClient
    case mbH of
        Just h -> do
            -- TODO catch exception
            hPutStrLn h (show clientDirective)
            hFlush h
        Nothing -> error "This is impossible"

getNextCommand :: IORef (Maybe Handle) -> Socket -> IO (Maybe (Command, [String]))
getNextCommand currentClient sock = do
    checkCurrent <- readIORef currentClient
    case checkCurrent of
        Just h -> hClose h
        Nothing -> return ()
    (h, _, _) <- accept sock
    writeIORef currentClient (Just h)
    msg <- hGetLine h -- TODO catch exception
    let serverDirective = readMaybe msg
    case serverDirective of
        Nothing -> do
            clientSend currentClient $ ClientUnexpectedError $
                "The client sent an invalid message to the server: " ++ show msg
            getNextCommand currentClient sock
        Just (SrvCommand cmd ghcOpts) -> do
            return $ Just (cmd, ghcOpts)
        Just SrvStatus -> do
            mapM_ (clientSend currentClient) $
                [ ClientStdout "Server is running."
                , ClientExit ExitSuccess
                ]
            getNextCommand currentClient sock
        Just SrvExit -> do
            mapM_ (clientSend currentClient) $
                [ ClientStdout "Shutting down server."
                , ClientExit ExitSuccess
                ]
            -- Must close the handle here because we are exiting the loop so it
            -- won't be closed in the code above
            hClose h
            return Nothing
