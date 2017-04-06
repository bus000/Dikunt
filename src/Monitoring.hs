{- |
 - Module      : Monitor
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
module Monitoring ( startMonitoring ) where

import System.Environment (getEnvironment)
import System.IO (hSetBuffering, BufferMode(..), Handle)
import System.Process (createProcess, std_out, std_in, proc, StdStream(..), createPipe, env, ProcessHandle)
import Control.Concurrent.MVar (MVar, newMVar)

data DikuntProcess = DikuntProcess
    { _location      :: FilePath
    , outputHandle  :: Handle
    , inputHandle   :: Handle
    , _processHandle :: ProcessHandle
    }

type Monitor = [DikuntProcess]

{- | Start a process for each file in the list of executable files given. Each
 - file is given a new stdin but all shares the same stdout. Whenever a program
 - crashes it is restarted by the monitor and the handles are updated. The
 - monitor runs in a separate thread to allow starting stopped processes in the
 - background. -}
startMonitoring :: [FilePath]
    -- ^ List of executable files.
    -> IO (MVar ([Handle], Handle))
startMonitoring execs = do
    processes <- startAll execs

    let ins = map inputHandle processes
        out = head $ map outputHandle processes

    newMVar (ins, out)

    -- TODO: Spawn thread that monitors the plugins.

{- | Start a process for each file given. -}
startAll :: [FilePath]
    -- ^ Files to execute.
    -> IO Monitor
startAll files = do
    environment <- getEnvironment
    (pipeRead, pipeWrite) <- createPipe -- Pipe to use as stdout.
    {-pipeReadH <- fdToHandle pipeRead-} -- TODO: REMOVE.
    {-pipeWriteH <- fdToHandle pipeWriteH-}

    let newEnv = ("PYTHONUNBUFFERED", "1"):environment

    mapM (start newEnv pipeWrite pipeRead) files
  where
    start environment houtWrite houtRead file = do
        (Just hin, _, _, processHandle) <- createProcess (proc file [])
            { std_out = UseHandle houtWrite
            , std_in = CreatePipe
            , env = Just environment
            }

        hSetBuffering hin LineBuffering

        return $ DikuntProcess file houtRead hin processHandle
