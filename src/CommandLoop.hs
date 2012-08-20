{-# LANGUAGE CPP #-}
module CommandLoop
    ( startCommandLoop
    ) where

import MonadUtils (MonadIO, liftIO)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import qualified ErrUtils
import qualified GHC
import qualified GHC.Paths
import qualified Outputable

import Types (ClientDirective(..), Command(..))
import Info (getIdentifierInfo, getType)

type CommandObj = (Command, [String])

type ClientSend = ClientDirective -> IO ()

startCommandLoop :: ClientSend -> IO (Maybe CommandObj) -> [String] -> Maybe Command -> IO ()
startCommandLoop clientSend getNextCommand initialGhcOpts mbInitial = do
    continue <- GHC.runGhc (Just GHC.Paths.libdir) $ do
        configOk <- GHC.gcatch (configSession clientSend initialGhcOpts >> return True)
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
    processNextCommand :: Bool -> GHC.Ghc (Maybe CommandObj)
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

    sendErrors :: GHC.Ghc () -> GHC.Ghc ()
    sendErrors action = GHC.gcatch action (\x -> handleConfigError x >> return ())

    handleConfigError :: GHC.GhcException -> GHC.Ghc Bool
    handleConfigError e = do
        liftIO $ mapM_ clientSend
            [ ClientStderr (GHC.showGhcException e "")
            , ClientExit (ExitFailure 1)
            ]
        return False

doMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
doMaybe Nothing _ = return ()
doMaybe (Just x) f = f x

configSession :: ClientSend -> [String] -> GHC.Ghc ()
configSession clientSend ghcOpts = do
    initialDynFlags <- GHC.getSessionDynFlags
    let updatedDynFlags = initialDynFlags
            { GHC.log_action = logAction clientSend
            , GHC.ghcLink = GHC.NoLink
            , GHC.hscTarget = GHC.HscInterpreted
            }
    (finalDynFlags, _, _) <- GHC.parseDynamicFlags updatedDynFlags (map GHC.noLoc ghcOpts)
    _ <- GHC.setSessionDynFlags finalDynFlags
    return ()

runCommand :: ClientSend -> Command -> GHC.Ghc ()
runCommand clientSend (CmdCheck file) = do
    let noPhase = Nothing
    target <- GHC.guessTarget file noPhase
    GHC.setTargets [target]
    let handler err = GHC.printException err >> return GHC.Failed
    flag <- GHC.handleSourceError handler (GHC.load GHC.LoadAllTargets)
    liftIO $ case flag of
        GHC.Succeeded -> clientSend (ClientExit ExitSuccess)
        GHC.Failed -> clientSend (ClientExit (ExitFailure 1))
runCommand clientSend (CmdInfo file identifier) = do
    result <- getIdentifierInfo file identifier
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
runCommand clientSend (CmdType file (line, col)) = do
    result <- getType file (line, col)
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
logAction :: ClientSend -> GHC.DynFlags -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.MsgDoc -> IO ()
logAction clientSend dflags severity srcspan style msg =
    let out = Outputable.renderWithStyle dflags fullMsg style
        _ = severity
    in clientSend (ClientStdout out)
    where fullMsg = ErrUtils.mkLocMessage severity srcspan msg
#else
logAction :: ClientSend -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.Message -> IO ()
logAction clientSend severity srcspan style msg =
    let out = Outputable.renderWithStyle fullMsg style
        _ = severity
    in clientSend (ClientStdout out)
    where fullMsg = ErrUtils.mkLocMessage srcspan msg
#endif
