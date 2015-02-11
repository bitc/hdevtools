{-# LANGUAGE CPP #-}
module CommandLoop
    ( newCommandLoopState
    , Config(..)
    , CabalConfig(..)
    , newConfig
    , startCommandLoop
    ) where

import Control.Monad (when)
import Data.IORef
import Data.List (find)
import Data.Traversable (traverse)
import MonadUtils (MonadIO, liftIO)
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath (takeDirectory)
import qualified ErrUtils
import qualified Exception (ExceptionMonad)
import qualified GHC
import qualified GHC.Paths
import qualified Outputable
import System.Posix.Types (EpochTime)
import System.Posix.Files (getFileStatus, modificationTime)

import Types (ClientDirective(..), Command(..), CommandExtra(..))
import Info (getIdentifierInfo, getType)
import Cabal (getPackageGhcOpts)

type ClientSend = ClientDirective -> IO ()

data State = State
    { stateWarningsEnabled :: Bool
    }

newCommandLoopState :: IO (IORef State)
newCommandLoopState = do
    newIORef $ State
        { stateWarningsEnabled = True
        }

data CabalConfig = CabalConfig
    { cabalConfigPath :: FilePath
    , cabalConfigOpts :: [String]
    , cabalConfigLastUpdatedAt :: EpochTime
    }
    deriving Eq

mkCabalConfig :: FilePath -> [String] -> IO CabalConfig
mkCabalConfig path opts = do
    fileStatus <- getFileStatus path
    return $ CabalConfig { cabalConfigPath = path
                         , cabalConfigOpts = opts
                         , cabalConfigLastUpdatedAt = modificationTime fileStatus
                         }

data Config = Config
    { configGhcOpts :: [String]
    , configCabal :: Maybe CabalConfig
    }
    deriving Eq

newConfig :: CommandExtra -> IO Config
newConfig cmdExtra = do
    mbCabalConfig <- traverse (\path -> mkCabalConfig path (ceCabalOptions cmdExtra)) $ ceCabalConfig cmdExtra
    return $ Config { configGhcOpts = ceGhcOptions cmdExtra
                    , configCabal = mbCabalConfig
                    }


type CommandObj = (Command, Config)

withWarnings :: (MonadIO m, Exception.ExceptionMonad m) => IORef State -> Bool -> m a -> m a
withWarnings state warningsValue action = do
    beforeState <- liftIO $ getWarnings
    liftIO $ setWarnings warningsValue
    action `GHC.gfinally`
        (liftIO $ setWarnings beforeState)
    where
    getWarnings :: IO Bool
    getWarnings = readIORef state >>= return . stateWarningsEnabled
    setWarnings :: Bool -> IO ()
    setWarnings val = modifyIORef state $ \s -> s { stateWarningsEnabled = val }

startCommandLoop :: IORef State -> ClientSend -> IO (Maybe CommandObj) -> Config -> Maybe Command -> IO ()
startCommandLoop state clientSend getNextCommand initialConfig mbInitialCommand = do
    continue <- GHC.runGhc (Just GHC.Paths.libdir) $ do
        configResult <- configSession state clientSend initialConfig
        case configResult of
          Left e -> do
              liftIO $ mapM_ clientSend
                  [ ClientStderr e
                  , ClientExit (ExitFailure 1)
                  ]
              processNextCommand True
          Right _ -> do
              doMaybe mbInitialCommand $ \cmd -> sendErrors (runCommand state clientSend cmd)
              processNextCommand False

    case continue of
        Nothing ->
            -- Exit
            return ()
        Just (cmd, config) -> startCommandLoop state clientSend getNextCommand config (Just cmd)
    where
    processNextCommand :: Bool -> GHC.Ghc (Maybe CommandObj)
    processNextCommand forceReconfig = do
        mbNextCmd <- liftIO getNextCommand
        case mbNextCmd of
            Nothing ->
                -- Exit
                return Nothing
            Just (cmd, config) ->
                if forceReconfig || (config /= initialConfig)
                    then return (Just (cmd, config))
                    else sendErrors (runCommand state clientSend cmd) >> processNextCommand False

    sendErrors :: GHC.Ghc () -> GHC.Ghc ()
    sendErrors action = GHC.gcatch action $ \e -> do
        liftIO $ mapM_ clientSend
            [ ClientStderr $ GHC.showGhcException e ""
            , ClientExit (ExitFailure 1)
            ]
        return ()

doMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
doMaybe Nothing _ = return ()
doMaybe (Just x) f = f x

configSession :: IORef State -> ClientSend -> Config -> GHC.Ghc (Either String ())
configSession state clientSend config = do
    eCabalGhcOpts <- case configCabal config of
                      Nothing ->
                          return $ Right []
                      Just cabalConfig -> do
                          liftIO $ setCurrentDirectory . takeDirectory $ cabalConfigPath cabalConfig
                          liftIO $ Cabal.getPackageGhcOpts (cabalConfigPath cabalConfig) (cabalConfigOpts cabalConfig)

    case eCabalGhcOpts of
      Left e -> return $ Left e
      Right cabalGhcOpts -> do
          let allGhcOpts = cabalGhcOpts ++ (configGhcOpts config)
          GHC.gcatch (fmap Right $ updateDynFlags allGhcOpts)
                     (fmap Left . handleGhcError)
  where
    updateDynFlags :: [String] -> GHC.Ghc ()
    updateDynFlags ghcOpts = do
        initialDynFlags <- GHC.getSessionDynFlags
        let updatedDynFlags = initialDynFlags
                { GHC.log_action = logAction state clientSend
                , GHC.ghcLink = GHC.NoLink
                , GHC.hscTarget = GHC.HscInterpreted
                }
        (finalDynFlags, _, _) <- GHC.parseDynamicFlags updatedDynFlags (map GHC.noLoc ghcOpts)
        _ <- GHC.setSessionDynFlags finalDynFlags
        return ()

    handleGhcError :: GHC.GhcException -> GHC.Ghc String
    handleGhcError e = return $ GHC.showGhcException e ""


runCommand :: IORef State -> ClientSend -> Command -> GHC.Ghc ()
runCommand _ clientSend (CmdCheck file) = do
    let noPhase = Nothing
    target <- GHC.guessTarget file noPhase
    GHC.setTargets [target]
    let handler err = GHC.printException err >> return GHC.Failed
    flag <- GHC.handleSourceError handler (GHC.load GHC.LoadAllTargets)
    liftIO $ case flag of
        GHC.Succeeded -> clientSend (ClientExit ExitSuccess)
        GHC.Failed -> clientSend (ClientExit (ExitFailure 1))
runCommand _ clientSend (CmdModuleFile moduleName) = do
    moduleGraph <- GHC.getModuleGraph
    case find (moduleSummaryMatchesModuleName moduleName) moduleGraph of
        Nothing ->
            liftIO $ mapM_ clientSend
                [ ClientStderr "Module not found"
                , ClientExit (ExitFailure 1)
                ]
        Just modSummary ->
            case GHC.ml_hs_file (GHC.ms_location modSummary) of
                Nothing ->
                    liftIO $ mapM_ clientSend
                        [ ClientStderr "Module does not have a source file"
                        , ClientExit (ExitFailure 1)
                        ]
                Just file ->
                    liftIO $ mapM_ clientSend
                        [ ClientStdout file
                        , ClientExit ExitSuccess
                        ]
    where
    moduleSummaryMatchesModuleName modName modSummary =
        modName == (GHC.moduleNameString . GHC.moduleName . GHC.ms_mod) modSummary
runCommand state clientSend (CmdInfo file identifier) = do
    result <- withWarnings state False $
        getIdentifierInfo file identifier
    case result of
        Left err ->
            liftIO $ mapM_ clientSend
                [ ClientStderr err
                , ClientExit (ExitFailure 1)
                ]
        Right info -> liftIO $ mapM_ clientSend
            [ ClientStdout info
            , ClientExit ExitSuccess
            ]
runCommand state clientSend (CmdType file (line, col)) = do
    result <- withWarnings state False $
        getType file (line, col)
    case result of
        Left err ->
            liftIO $ mapM_ clientSend
                [ ClientStderr err
                , ClientExit (ExitFailure 1)
                ]
        Right types -> liftIO $ do
            mapM_ (clientSend . ClientStdout . formatType) types
            clientSend (ClientExit ExitSuccess)
    where
    formatType :: ((Int, Int, Int, Int), String) -> String
    formatType ((startLine, startCol, endLine, endCol), t) =
        concat
            [ show startLine , " "
            , show startCol , " "
            , show endLine , " "
            , show endCol , " "
            , "\"", t, "\""
            ]

#if __GLASGOW_HASKELL__ >= 706
logAction :: IORef State -> ClientSend -> GHC.DynFlags -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.MsgDoc -> IO ()
logAction state clientSend dflags severity srcspan style msg =
    let out = Outputable.renderWithStyle dflags fullMsg style
        _ = severity
    in logActionSend state clientSend severity out
    where fullMsg = ErrUtils.mkLocMessage severity srcspan msg
#else
logAction :: IORef State -> ClientSend -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.Message -> IO ()
logAction state clientSend severity srcspan style msg =
    let out = Outputable.renderWithStyle fullMsg style
        _ = severity
    in logActionSend state clientSend severity out
    where fullMsg = ErrUtils.mkLocMessage srcspan msg
#endif

logActionSend :: IORef State -> ClientSend -> GHC.Severity -> String -> IO ()
logActionSend state clientSend severity out = do
    currentState <- readIORef state
    when (not (isWarning severity) || stateWarningsEnabled currentState) $
        clientSend (ClientStdout out)
    where
    isWarning :: GHC.Severity -> Bool
    isWarning GHC.SevWarning = True
    isWarning _ = False
