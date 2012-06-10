module CommandLoop where

import Data.Time (getCurrentTime)
import ErrUtils (Message, mkLocMessage)
import GHC (Ghc, HscTarget(HscInterpreted), LoadHowMuch(LoadAllTargets), Severity, SrcSpan, SuccessFlag(Succeeded, Failed), getSessionDynFlags, guessTarget, handleSourceError, hscTarget, load, log_action, noLoc, parseDynamicFlags, printException, runGhc, setSessionDynFlags, setTargets)
import GHC.Paths (libdir)
import MonadUtils (MonadIO, liftIO)
import Outputable (PprStyle, renderWithStyle)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Types (ClientDirective(..), Command(..))

type CommandObj = (Command, [String])

type ClientSend = ClientDirective -> IO ()

startCommandLoop :: ClientSend -> IO (Maybe CommandObj) -> [String] -> Maybe Command -> IO ()
startCommandLoop clientSend getNextCommand initialGhcOpts mbInitial = do
    let processNextCommand :: Ghc (Maybe CommandObj)
        processNextCommand = do
            mbNextCmd <- liftIO getNextCommand
            case mbNextCmd of
                Nothing ->
                    -- Exit
                    return Nothing
                Just (cmd, ghcOpts) ->
                    if ghcOpts /= initialGhcOpts
                        then return (Just (cmd, ghcOpts))
                        else runCommand clientSend cmd >> processNextCommand

    continue <- runGhc (Just libdir) $ do
        configSession clientSend initialGhcOpts
        case mbInitial of
            Just initialCmd -> runCommand clientSend initialCmd
            Nothing -> return ()
        processNextCommand

    case continue of
        Nothing ->
            -- Exit
            return ()
        Just (cmd, ghcOpts) -> startCommandLoop clientSend getNextCommand ghcOpts (Just cmd)

configSession :: ClientSend -> [String] -> Ghc ()
configSession clientSend ghcOpts = do
    info "Reading initial DynFlags"
    initialDynFlags <- getSessionDynFlags
    let updatedDynFlags = initialDynFlags
            { log_action = logAction clientSend
            , hscTarget = HscInterpreted
            }
    info "Parsing ghcOpts DynFlags"
    (finalDynFlags, _, _) <- parseDynamicFlags updatedDynFlags (map noLoc ghcOpts)
    info "Setting final DynFlags"
    _ <- setSessionDynFlags finalDynFlags
    return ()

runCommand :: ClientSend -> Command -> Ghc ()
runCommand clientSend (CmdCheck file) = do
    info $ "Guessing Target: " ++ file
    let noPhase = Nothing
    target <- guessTarget file noPhase
    info "Setting target list"
    setTargets [target]
    info "Loading all targets"
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


info :: MonadIO m => String -> m ()
info msg = liftIO $ do
    now <- getCurrentTime
    putStrLn $ show now ++ " " ++ msg
