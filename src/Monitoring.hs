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
module Monitoring
    ( startMonitoring

    -- DikuntProcess type and getters and setters.
    , DikuntProcess
    , location
    , outputHandle
    , inputHandle
    , processHandle

    -- Monitor type.
    , Monitor(..)
    ) where

import System.Environment (getEnvironment)
import System.IO (hSetBuffering, BufferMode(..), Handle, hPutStrLn, stderr)
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)
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
    )
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import GHC.IO.Handle (hDuplicate)

data DikuntProcess = DikuntProcess
    { location      :: FilePath
    , outputHandle  :: Handle
    , inputHandle   :: Handle
    , processHandle :: ProcessHandle
    }

type Pipe = (Handle, Handle)
data Monitor = Monitor [DikuntProcess] Pipe

{- | Start a process for each file in the list of executable files given. Each
 - file is given a new stdin but all shares the same stdout. Whenever a program
 - crashes it is restarted by the monitor and the handles are updated. The
 - monitor runs in a separate thread to allow starting stopped processes in the
 - background. -}
startMonitoring :: [FilePath]
    -- ^ List of executable files.
    -> [String]
    -- ^ Arguments.
    -> IO (MVar Monitor)
startMonitoring execs args = do
    monitor <- startAll execs args >>= \m -> newMVar m

    _ <- forkIO $ monitorProcesses monitor args

    return monitor

{- | Monitor all processes in monitor restarting them whenever they stop. The
 - monitor reports the error code of the process to stderr before it is
 - restarted. The processes are restarted every 30 seconds (TODO: configure
 - sleep time). -}
monitorProcesses :: MVar Monitor
    -- ^ The monitor to run.
    -> [String]
    -- ^ List of arguments to give to new processes.
    -> IO ()
monitorProcesses monitorMVar args = forever $ do
    threadDelay 30000000 -- Delay 30 seconds.
    Monitor processes pipe <- takeMVar monitorMVar
    processes' <- mapM (restartStopped pipe) processes
    putMVar monitorMVar $ Monitor processes' pipe
  where
    restartStopped pipe process@(DikuntProcess loc _ _ pH) = do
        exitCodeMay <- getProcessExitCode pH
        case exitCodeMay of
            Just code -> do
                hPutStrLn stderr $ loc ++ " exited with exit code " ++ show code
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

    return $ Monitor processes pipe

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
