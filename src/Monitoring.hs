{- |
 - Module      : Monitoring
 - Description : Monitor execution of Dikunt plugins.
 - Copyright   : (c) Magnus Stavngaard, 2016
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - Responsible for opening and maintaining a running copy of each executable
 - given.
 -}
{-# LANGUAGE OverloadedStrings #-}
module Monitoring
    (
    -- | Functions for creating handling and stopping monitors.
      startMonitoring
    , writeAll
    , readContent
    , stopMonitoring

    -- Monitor type.
    , DikuntMonitor
    ) where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Control.Concurrent (forkIO, threadDelay, ThreadId, killThread)
import Control.Exception (catch, IOException)
import Control.Monad (forever)
import System.Environment (getEnvironment)
import System.IO (hSetBuffering, BufferMode(..), Handle, hClose)
import qualified System.Log.Logger as Log
import System.Process
    ( createProcess
    , std_out
    , std_in
    , proc
    , StdStream(..)
    , createPipe
    , env
    , ProcessHandle
    , getProcessExitCode
    , waitForProcess
    , terminateProcess
    )
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, modifyMVar_, withMVar)
import GHC.IO.Handle (hDuplicate)

data DikuntProcess = DikuntProcess
    { location      :: FilePath
    , outputHandle  :: Handle
    , inputHandle   :: Handle
    , processHandle :: ProcessHandle
    }

type DikuntMonitor = MVar Monitor

type Pipe = (Handle, Handle)

data Monitor = SetupMonitor [DikuntProcess] Pipe
    | Monitor [DikuntProcess] Pipe ThreadId

{- | Start a process for each file in the list of executable files given. Each
 - file is given a new stdin but all shares the same stdout. Whenever a program
 - crashes it is restarted by the monitor and the handles are updated. The
 - monitor runs in a separate thread to allow starting stopped processes in the
 - background. -}
startMonitoring :: [FilePath]
    -- ^ List of executable files.
    -> [String]
    -- ^ Arguments.
    -> IO DikuntMonitor
startMonitoring execs args = do
    monitor <- startAll execs args >>= \m -> newMVar m

    monitorId <- forkIO $ monitorProcesses monitor args
    modifyMVar_ monitor $ \m -> return $ setThreadId m monitorId

    return monitor

stopMonitoring :: DikuntMonitor -> IO ()
stopMonitoring monitor = withMVar monitor $ \m ->
    stopMonitor m >> closeProcesses $ getProcesses m
  where
    stopMonitor (Monitor _ _ monitorId) = killThread monitorId
    stopMonitor (SetupMonitor _ _) = return ()

    closeProcesses procs =
        mapM_ waitClose $ map (extractTwo processHandle location) procs

    waitClose (h, loc) = do
        Log.infoM "monitoring.stopMonitoring" $ "Waiting for plugin \"" ++
            loc ++ "\" to close down..."
        terminateProcess h
        waitForProcess h

writeAll :: DikuntMonitor -> T.Text -> IO ()
writeAll monitor message = withMVar monitor $ \m -> do
    mapM_ (safePrint message) $ (map inputHandle . getProcesses) m
  where
    safePrint msg h = T.hPutStrLn h msg `catch` (\e ->
        Log.errorM "monitoring.handleMessage" $ show (e :: IOException))

readContent :: DikuntMonitor -> IO T.Text
readContent monitor = do
    handles <- withMVar monitor $ \m -> return (map outputHandle $ getProcesses m)
    case handles of
        [] -> return ""
        (h:_) -> T.hGetContents h

{- | Monitor all processes in monitor restarting them whenever they stop. The
 - monitor reports the error code of the process to the log before it is
 - restarted. The processes are restarted every 30 seconds (TODO: configure
 - sleep time). -}
monitorProcesses :: MVar Monitor
    -- ^ The monitor to run.
    -> [String]
    -- ^ List of arguments to give to new processes.
    -> IO ()
monitorProcesses monitorMVar args = forever $ do
    threadDelay 30000000 -- Delay 30 seconds.
    monitor <- takeMVar monitorMVar
    let (processes, pipe) = (getProcesses monitor, getPipe monitor)
    processes' <- mapM (restartStopped pipe) processes
    putMVar monitorMVar $ setProcesses monitor processes'
  where
    restartStopped pipe process@(DikuntProcess loc _ _ pH) = do
        exitCodeMay <- getProcessExitCode pH
        case exitCodeMay of
            Just code -> do
                Log.errorM "monitoring.monitorProcesses" $ loc ++
                    " execited with exit code " ++ show code
                start pipe args loc
            Nothing -> return process

{- | Start a process for each file given. -}
startAll :: [FilePath]
    -- ^ Files to execute.
    -> [String]
    -- ^ Arguments.
    -> IO Monitor
startAll files args = do
    pipe <- createPipe -- Pipe to use as stdout.
    processes <- mapM (start pipe args) files

    return $ SetupMonitor processes pipe

{- | Starts a process. start (hRead, hWrite) args file - Starts the executable
 - file 'file' with the arguments 'args' and use (hRead, hWrite) as the output
 - pipe of the new process. -}
start :: Pipe
    -- ^ Output pipe for new process.
    -> [String]
    -- ^ Arguments to executable.
    -> FilePath
    -- ^ Path to executable file.
    -> IO DikuntProcess
start (houtRead, houtWrite) args file = do
    {- Don't buffer python stdin and stdout. -}
    environment <- fmap (\e -> ("PYTHONUNBUFFERED", "1"):e) getEnvironment
    dupHoutWrite <- hDuplicate houtWrite

    (Just hin, _, _, procHandle) <- createProcess (proc file args)
        { std_out = UseHandle dupHoutWrite
        , std_in = CreatePipe
        , env = Just environment
        }

    hSetBuffering hin LineBuffering

    return $ DikuntProcess file houtRead hin procHandle

getProcesses :: Monitor -> [DikuntProcess]
getProcesses (SetupMonitor processes _) = processes
getProcesses (Monitor processes _ _) = processes

setProcesses :: Monitor -> [DikuntProcess] -> Monitor
setProcesses (SetupMonitor _ pipe) procs = SetupMonitor procs pipe
setProcesses (Monitor _ pipe monitorId) procs = Monitor procs pipe monitorId

getPipe :: Monitor -> Pipe
getPipe (SetupMonitor _ pipe) = pipe
getPipe (Monitor _ pipe _) = pipe

setThreadId :: Monitor -> ThreadId -> Monitor
setThreadId (SetupMonitor procs pipe) = Monitor procs pipe
setThreadId (Monitor procs pipe _) = Monitor procs pipe

extractTwo :: (a -> b) -> (a -> c) -> a -> (b, c)
extractTwo f g x = (f x, g x)
