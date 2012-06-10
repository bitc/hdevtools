module CommandLoop where

import ErrUtils (Message, mkLocMessage)
import GHC (Ghc, GhcException, GhcLink(NoLink), HscTarget(HscInterpreted), LoadHowMuch(LoadAllTargets), Severity, SrcSpan, SuccessFlag(Succeeded, Failed), gcatch, getSessionDynFlags, ghcLink, guessTarget, handleSourceError, hscTarget, load, log_action, noLoc, parseDynamicFlags, printException, runGhc, setSessionDynFlags, setTargets, showGhcException)
import GHC.Paths (libdir)
import MonadUtils (MonadIO, liftIO)
import Outputable (PprStyle, renderWithStyle)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Types (ClientDirective(..), Command(..))

type CommandObj = (Command, [String])

type ClientSend = ClientDirective -> IO ()

startCommandLoop :: ClientSend -> IO (Maybe CommandObj) -> [String] -> Maybe Command -> IO ()
startCommandLoop clientSend getNextCommand initialGhcOpts mbInitial = do
    continue <- runGhc (Just libdir) $ do
        configOk <- gcatch (configSession clientSend initialGhcOpts >> return True)
            handleConfigError
        if configOk
            then do
                doMaybe mbInitial $ \cmd -> sendErrors (runCommand clientSend cmd)
                processNextCommand False
            else processNextCommand True

    case continue of
        Nothing ->
            -- Exit
            return ()
        Just (cmd, ghcOpts) -> startCommandLoop clientSend getNextCommand ghcOpts (Just cmd)
    where
    processNextCommand :: Bool -> Ghc (Maybe CommandObj)
    processNextCommand forceReconfig = do
        mbNextCmd <- liftIO getNextCommand
        case mbNextCmd of
            Nothing ->
                -- Exit
                return Nothing
            Just (cmd, ghcOpts) ->
                if forceReconfig || (ghcOpts /= initialGhcOpts)
                    then return (Just (cmd, ghcOpts))
                    else sendErrors (runCommand clientSend cmd) >> processNextCommand False

    sendErrors :: Ghc () -> Ghc ()
    sendErrors action = gcatch action (\x -> handleConfigError x >> return ())

    handleConfigError :: GhcException -> Ghc Bool
    handleConfigError e = do
        liftIO $ mapM_ clientSend
            [ ClientStderr (showGhcException e "")
            , ClientExit (ExitFailure 1)
            ]
        return False

doMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
doMaybe Nothing _ = return ()
doMaybe (Just x) f = f x

configSession :: ClientSend -> [String] -> Ghc ()
configSession clientSend ghcOpts = do
    initialDynFlags <- getSessionDynFlags
    let updatedDynFlags = initialDynFlags
            { log_action = logAction clientSend
            , ghcLink = NoLink
            , hscTarget = HscInterpreted
            }
    (finalDynFlags, _, _) <- parseDynamicFlags updatedDynFlags (map noLoc ghcOpts)
    _ <- setSessionDynFlags finalDynFlags
    return ()

runCommand :: ClientSend -> Command -> Ghc ()
runCommand clientSend (CmdCheck file) = do
    let noPhase = Nothing
    target <- guessTarget file noPhase
    setTargets [target]
    let handler err = printException err >> return Failed
    flag <- handleSourceError handler (load LoadAllTargets)
    liftIO $ case flag of
        Succeeded -> clientSend (ClientExit ExitSuccess)
        Failed -> clientSend (ClientExit (ExitFailure 1))

logAction :: ClientSend -> Severity -> SrcSpan -> PprStyle -> Message -> IO ()
logAction clientSend severity srcspan style msg =
    let out = renderWithStyle fullMsg style
        _ = severity
    in clientSend (ClientStdout out)
    where fullMsg = mkLocMessage srcspan msg
