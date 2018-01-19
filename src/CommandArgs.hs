{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module CommandArgs
    ( HDevTools(..)
    , loadHDevTools
    )
where

import System.Console.CmdArgs.Implicit
import System.Environment (getProgName)
import System.Info (arch, os)
import qualified Config

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

cabalVersion :: String
cabalVersion =
#ifdef ENABLE_CABAL
    "cabal-" ++ VERSION_Cabal
#else
    "no cabal support"
#endif

fullVersion :: String
fullVersion =
    concat
        [ programVersion
        , " ("
        , "ghc-", Config.cProjectVersion, "-", arch, "-", os
        , ", ", cabalVersion
        , ")"
        ]

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
        , cabalOpts :: [String]
        , file :: String
        }
    | ModuleFile
        { socket :: Maybe FilePath
        , ghcOpts :: [String]
        , cabalOpts :: [String]
        , module_ :: String
        }
    | Info
        { socket :: Maybe FilePath
        , ghcOpts :: [String]
        , cabalOpts :: [String]
        , file :: String
        , identifier :: String
        }
    | Type
        { socket :: Maybe FilePath
        , ghcOpts :: [String]
        , cabalOpts :: [String]
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
    , cabalOpts = []
    , file = ""
    }

dummyModuleFile :: HDevTools
dummyModuleFile = ModuleFile
    { socket = Nothing
    , ghcOpts = []
    , cabalOpts = []
    , module_ = ""
    }

dummyInfo :: HDevTools
dummyInfo = Info
    { socket = Nothing
    , ghcOpts = []
    , cabalOpts = []
    , file = ""
    , identifier = ""
    }

dummyType :: HDevTools
dummyType = Type
    { socket = Nothing
    , ghcOpts = []
    , cabalOpts = []
    , file = ""
    , line = 0
    , col = 0
    }

admin :: Annotate Ann
admin = record dummyAdmin
    [ socket   := def += typFile += help "socket file to use"
    , start_server   := def            += help "start server"
    , noDaemon := def            += help "do not daemonize (only if --start-server)"
    , status   := def            += help "show status of server"
    , stop_server := def         += help "shutdown the server"
    ] += help "Interactions with the server"

check :: Annotate Ann
check = record dummyCheck
    [ socket   := def += typFile += help "socket file to use"
    , ghcOpts  := def += typ "OPTION"   += help "ghc options"
#ifdef ENABLE_CABAL
    , cabalOpts := def += typ "OPTION"  += help "cabal options"
#else
    , cabalOpts := def += ignore
#endif
    , file     := def += typFile      += argPos 0 += opt ""
    ] += help "Check a haskell source file for errors and warnings"

moduleFile :: Annotate Ann
moduleFile = record dummyModuleFile
    [ socket   := def += typFile += help "socket file to use"
    , ghcOpts  := def += typ "OPTION" += help "ghc options"
#ifdef ENABLE_CABAL
    , cabalOpts := def += typ "OPTION"  += help "cabal options"
#else
    , cabalOpts := def += ignore
#endif
    , module_  := def += typ "MODULE" += argPos 0
    ] += help "Get the haskell source file corresponding to a module name"

info :: Annotate Ann
info = record dummyInfo
    [ socket     := def += typFile += help "socket file to use"
    , ghcOpts    := def += typ "OPTION" += help "ghc options"
#ifdef ENABLE_CABAL
    , cabalOpts := def += typ "OPTION"  += help "cabal options"
#else
    , cabalOpts := def += ignore
#endif
    , file       := def += typFile      += argPos 0 += opt ""
    , identifier := def += typ "IDENTIFIER" += argPos 1
    ] += help "Get info from GHC about the specified identifier"

type_ :: Annotate Ann
type_ = record dummyType
    [ socket   := def += typFile += help "socket file to use"
    , ghcOpts  := def += typ "OPTION" += help "ghc options"
#ifdef ENABLE_CABAL
    , cabalOpts := def += typ "OPTION"  += help "cabal options"
#else
    , cabalOpts := def += ignore
#endif
    , file     := def += typFile      += argPos 0 += opt ""
    , line     := def += typ "LINE"   += argPos 1
    , col      := def += typ "COLUMN" += argPos 2
    ] += help "Get the type of the expression at the specified line and column"

full :: String -> Annotate Ann
full progName = modes_ [admin += auto, check, moduleFile, info, type_]
        += helpArg [name "h", groupname "Help"]
        += versionArg [groupname "Help"]
        += program progName
        += summary (progName ++ ": " ++ fullVersion)

loadHDevTools :: IO HDevTools
loadHDevTools = do
    progName <- getProgName
    (cmdArgs_ (full progName) :: IO HDevTools)
