{-# LANGUAGE CPP #-}

module Daemonize
    ( daemonize
    ) where

import Control.Monad (when, void)
import System.Exit (ExitCode(ExitSuccess))
#ifdef mingw32_HOST_OS
import System.Environment
import System.Exit (exitSuccess)
import System.Process
#else
import System.Posix.Process (exitImmediately, createSession, forkProcess)
import System.Posix.IO
#endif

import Server (createListenSocket, startServer)

-- | This goes against the common daemon guidelines and does not change the
-- current working directory!
--
-- We need the daemon to stay in the current directory for the GHC API to work
daemonize :: Bool -> FilePath -> IO ()
#ifdef mingw32_HOST_OS
daemonize exit sock = do
    exePath <- getExecutablePath
    void $ createProcess $ (proc exePath ["admin", "--socket=" ++ sock, "--start-server", "-n"]) {
        close_fds = True }
    when exit exitSuccess
#else
daemonize exit sock = do
    s <- createListenSocket sock
    _ <- forkProcess child1
    when exit $ exitImmediately ExitSuccess

    where
        child1 = do
            _ <- createSession
            _ <- forkProcess child2
            exitImmediately ExitSuccess

        child2 = do
            mapM_ closeFd [stdInput, stdOutput, stdError]
            nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
            mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
            closeFd nullFd
            startServer sock (Just s)
#endif
