module Types
    ( ServerDirective(..)
    , ClientDirective(..)
    , Command(..)
    ) where

import System.Exit (ExitCode)

data ServerDirective
    = SrvCommand Command [String]
    | SrvStatus
    | SrvExit
    deriving (Read, Show)

data ClientDirective
    = ClientStdout String
    | ClientStderr String
    | ClientExit ExitCode
    | ClientUnexpectedError String -- ^ For unexpected errors that should not happen
    deriving (Read, Show)

data Command
    = CmdCheck FilePath
    | CmdModuleFile String
    | CmdInfo FilePath String
    | CmdType FilePath (Int, Int)
    | CmdFindSymbol String [String]
    deriving (Read, Show)
