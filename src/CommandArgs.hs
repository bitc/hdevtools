{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module CommandArgs
    ( HDevTools(..)
    , loadHDevTools
    )
where

import System.Console.CmdArgs.Implicit
import System.Environment (getProgName)

#ifdef CABAL
import Data.Version (showVersion)
import Paths_hdevtools (version)
#endif

programVersion :: String
programVersion =
#ifdef CABAL
    "version " ++ showVersion version
#else
    "unknown-version (not built with cabal)"
#endif

data HDevTools
    = Admin
        { socket :: Maybe FilePath
        , start_server :: Bool
        , noDaemon :: Bool
        , status :: Bool
        , stop_server :: Bool
        }
    | Check
        { socket :: Maybe FilePath
        , ghcOpts :: [String]
        , file :: String
        }
    | Type
        { socket :: Maybe FilePath
        , ghcOpts :: [String]
        , file :: String
        , line :: Int
        , col :: Int
        }
    deriving (Show, Data, Typeable)

dummyAdmin :: HDevTools
dummyAdmin = Admin
    { socket = Nothing
    , start_server = False
    , noDaemon = False
    , status = False
    , stop_server = False
    }

dummyCheck :: HDevTools
dummyCheck = Check
    { socket = Nothing
    , ghcOpts = []
    , file = ""
    }

dummyType :: HDevTools
dummyType = Type
    { socket = Nothing
    , ghcOpts = []
    , file = ""
    , line = 0
    , col = 0
    }

admin :: Annotate Ann
admin = record dummyAdmin
    [ socket   := def += typFile += help "socket file to use"
    , start_server   := def            += help "start server"
    , noDaemon := def            += help "do not daemonize (only if --server)"
    , status   := def            += help "show status of server"
    , stop_server := def         += help "shutdown the server"
    ] += help "Interactions with the server"

check :: Annotate Ann
check = record dummyCheck
    [ socket   := def += typFile += help "socket file to use"
    , ghcOpts  := def += typ "OPTION"   += help "ghc options"
    , file     := def += typFile      += argPos 0 += opt ""
    ] += help "Check a haskell source file for errors and warnings"

type_ :: Annotate Ann
type_ = record dummyType
    [ socket   := def += typFile += help "socket file to use"
    , ghcOpts  := def += typ "OPTION" += help "ghc options"
    , file     := def += typFile      += argPos 0 += opt ""
    , line     := def += typ "LINE"   += argPos 1
    , col      := def += typ "COLUMN" += argPos 2
    ] += help "Get the type of the expression at the specified line and column"

full :: String -> Annotate Ann
full progName = modes_ [admin += auto, check, type_]
        += verbosity
        += helpArg [name "h", groupname "Help"]
        += versionArg [groupname "Help"]
        += program progName
        += summary (progName ++ ": " ++ programVersion)

loadHDevTools :: IO HDevTools
loadHDevTools = do
    progName <- getProgName
    (cmdArgs_ (full progName) :: IO HDevTools)
