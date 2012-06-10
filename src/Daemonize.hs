module Daemonize
    ( daemonize
    ) where

import Control.Monad (when)
import System.Exit (ExitCode(ExitSuccess))
import System.Posix.Process (exitImmediately, createSession, forkProcess)
import System.Posix.IO

-- | This goes against the common daemon guidelines and does not change the
-- current working directory!
--
-- We need the daemon to stay in the current directory for the GHC API to work
daemonize :: Bool -> IO () -> IO ()
daemonize exit program = do
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
        program
