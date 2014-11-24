{-# LANGUAGE CPP #-}

module Util
    ( readMaybe
    , createListenSocket
    , connect
    ) where

import Network
import System.IO (Handle)

-- Taken from:
-- http://stackoverflow.com/questions/8066850/why-doesnt-haskells-prelude-read-return-a-maybe/8080084#8080084
readMaybe        :: (Read a) => String -> Maybe a
readMaybe s      =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> Just x
                         _   -> Nothing

createListenSocket :: FilePath -> IO Socket
#ifdef mingw32_HOST_OS
createListenSocket socketPath =
    listenOn (PortNumber $ fromInteger $ read socketPath)
#else
createListenSocket socketPath =
    listenOn (UnixSocket socketPath)
#endif

connect :: FilePath -> IO Handle
#ifdef mingw32_HOST_OS
connect sock = do
    connectTo "" (PortNumber $ fromInteger $ read sock)
#else
connect sock = do
    connectTo "" (UnixSocket sock)
#endif
