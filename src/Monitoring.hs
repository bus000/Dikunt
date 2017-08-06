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

import Control.Concurrent (forkIO, threadDelay, ThreadId, killThread)
import Control.Exception (catch, IOException)
import Control.Monad (forever)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Environment (getEnvironment)
import System.IO (hSetBuffering, BufferMode(..), Handle)
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

{- | Represent a Dikunt process. Each process contains a path to the executable
 - a stdin, stdout and process handle. -}
data DikuntProcess = DikuntProcess
    { location      :: FilePath -- ^ Location of executable.
    , outputHandle  :: Handle -- ^ File handle for process output.
    , inputHandle   :: Handle -- ^ File handle for process input.
    , processHandle :: ProcessHandle -- ^ Handle for process.
    }

{- | Main type of module. Handler for Dikunt plugins. When a monitor is
 - constructed the plugins are started and a thread is started that keeps track
 - of whether any plugin has stopped. If any plugin is stopped at any time it is
 - restarted. When to monitor is stopped the thread monitoring the plugins are
 - killed and the plugins are killed. -}
type DikuntMonitor = MVar Monitor

{- | Unix pipe (readend, writend). -}
type Pipe = (Handle, Handle)

{- | Internal representation of a Dikunt monitor. -}
data Monitor
    -- | Before monitoring thread is started the monitor is in this state.
    = SetupMonitor [DikuntProcess] Pipe
    -- | After monitoring thread is started the monitor is in this state.
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

{- | Stop thread monitoring Dikunt plugins and stop the plugins. The function
 - blocks until all plugins are shut down. -}
stopMonitoring :: DikuntMonitor
    -- ^ The monitor to shut down.
    -> IO ()
stopMonitoring monitor = withMVar monitor $ \m ->
    stopMonitor m >> closeProcesses (getProcesses m)
  where
    stopMonitor (Monitor _ _ monitorId) = killThread monitorId
    stopMonitor (SetupMonitor _ _) = return ()

    closeProcesses = mapM_ $ waitClose . extractTwo processHandle location

    waitClose (h, loc) = do
        lInfo $ "Waiting for plugin \"" ++ loc ++ "\" to close down..."
        terminateProcess h
        waitForProcess h

    lInfo = Log.infoM "monitoring.stopMonitoring"

{- | Write a message to all dikunt plugings in the monitor. -}
writeAll :: DikuntMonitor
    -- The monitor specifying the plugins.
    -> T.Text
    -- The message to write.
    -> IO ()
writeAll monitor message = withMVar monitor $ \m ->
    mapM_ (safePrint message . inputHandle) (getProcesses m)
  where
    safePrint msg h = T.hPutStrLn h msg `catch` (\e ->
        lError $ show (e :: IOException))

    lError = Log.errorM "monitoring.writeAll"

{- | Read all output that comes from any plugin in the monitor. -}
readContent :: DikuntMonitor
    -- ^ The monitor to read from.
    -> IO T.Text
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

{- | Start a process for each files given. -}
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

{- | Get the processes managed by the monitor. -}
getProcesses :: Monitor
    -- ^ The monitor to get the processes from.
    -> [DikuntProcess]
getProcesses (SetupMonitor processes _) = processes
getProcesses (Monitor processes _ _) = processes

{- | Change the processes in a monitor. -}
setProcesses :: Monitor
    -- ^ Monitor to change processes in.
    -> [DikuntProcess]
    -- ^ The processes to change to.
    -> Monitor
setProcesses (SetupMonitor _ pipe) procs = SetupMonitor procs pipe
setProcesses (Monitor _ pipe monitorId) procs = Monitor procs pipe monitorId

{- | Get pipe from monitor. -}
getPipe :: Monitor
    -- ^ The monitor to get the pipe from.
    -> Pipe
getPipe (SetupMonitor _ pipe) = pipe
getPipe (Monitor _ pipe _) = pipe

{- | Set the ID of the monitoring thread in the monitor. -}
setThreadId :: Monitor
    -- ^ Monitor to change.
    -> ThreadId
    -- ^ ThreadId to change to.
    -> Monitor
setThreadId (SetupMonitor procs pipe) = Monitor procs pipe
setThreadId (Monitor procs pipe _) = Monitor procs pipe

{- | Utility function to call two functions on an argument and return both
 - results.-}
extractTwo :: (a -> b)
    -- ^ First function to call.
    -> (a -> c)
    -- ^ Second function to call.
    -> a
    -- ^ Value to call function on.
    -> (b, c)
extractTwo f g x = (f x, g x)
