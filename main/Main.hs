{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Bot (connect, disconnect, runBot)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Configurator (load, Worth(..), require)
import Data.List (intercalate)
import Data.Version (showVersion)
import Paths_Dikunt (getDataFileName, version)
import qualified System.Console.CmdArgs as CMD
import System.Console.CmdArgs ((&=))
import System.Environment (getEnv, setEnv)
import System.IO (stderr, hPutStrLn)
import qualified System.Log.Formatter as Log
import qualified System.Log.Handler as Log
import qualified System.Log.Handler.Simple as Log
import qualified System.Log.Logger
import qualified System.Log.Logger as Log
import qualified Types.BotTypes as BT

data Dikunt = Dikunt
    { server          :: String
    , nickname        :: String
    , password        :: String
    , channel         :: String
    , port            :: Integer
    , pluginArgs      :: [String]
    , withPlugins     :: [FilePath]
    , withoutPlugins  :: [FilePath]
    , pluginLocations :: [FilePath]
    } deriving (CMD.Data, CMD.Typeable, Show, Eq)

dikunt :: String -> Dikunt
dikunt vers = Dikunt
    { server = "irc.freenode.org" &= CMD.help "Server to connect to"
    , nickname = "dikunt" &= CMD.help "Nick to use"
    , password = CMD.def &= CMD.help "Password to use"
    , channel = "#dikufags" &= CMD.help "Channel to connect to"
    , port = 6667 &= CMD.help "Port to connect to"
    , pluginArgs = [] &= CMD.explicit &= CMD.name "plugin-arg" &=
        CMD.help "Argument that is passed to all plugins started by Dikunt."
    , withPlugins = [] &= CMD.explicit &= CMD.name "with-plugin" &=
        CMD.help "Path to plugins to run besides what is specified in the \
            \configuration"
    , withoutPlugins = [] &= CMD.explicit &= CMD.name "without-plugin" &=
        CMD.help "Path to plugins to ignore i.e. they are not run even though \
            \they are specified in the configuration"
    , pluginLocations = [] &= CMD.explicit &= CMD.name "plugin-location" &=
        CMD.help "Path to folder containing plugins that are not on the \
            \current $PATH"
    } &=
        CMD.help "Bot to run on IRC channels" &=
        CMD.summary ("Dikunt v" ++ vers ++ " (C) Magnus Stavngaard") &=
        CMD.helpArg [CMD.explicit, CMD.name "help", CMD.name "h"] &=
        CMD.versionArg [CMD.explicit, CMD.name "version", CMD.name "v"]

getBotConfig :: Dikunt -> Maybe BT.BotConfig
getBotConfig (Dikunt serv nick pass chan p _ _ _ _) =
    BT.botConfig serv nick pass chan p

{- | Allocate resources used by the bot. Setup logging and connection to IRC
 - server. -}
setup :: BT.BotConfig
    -- ^ Configuration of bot.
    -> [FilePath]
    -- ^ List of plugins to startup.
    -> [String]
    -- ^ List of arguments to plugins.
    -> [FilePath]
    -- ^ List of data files the program uses.
    -> IO BT.Bot
setup conf plugins args dfiles = do
    -- Remove default stderr logging.
    Log.updateGlobalLogger Log.rootLoggerName Log.removeHandler

    -- Setup stderr log for errors.
    handleErr <- Log.streamHandler stderr Log.ERROR >>= \lh ->
        return $ Log.setFormatter lh logFormatter
    Log.updateGlobalLogger Log.rootLoggerName (Log.addHandler handleErr)

    -- Setup file log for received messages.
    messageLog <- getDataFileName "data/msgLog.db"
    handleMsg <- Log.fileHandler messageLog Log.INFO >>= \lh ->
        return $ Log.setFormatter lh logFormatter
    Log.updateGlobalLogger "messages" (Log.addHandler handleMsg)
    Log.updateGlobalLogger "messages" (System.Log.Logger.setLevel Log.INFO)

    -- Setup file log for received privmsgs.
    privMsgLog <- getDataFileName "data/privmsgLog.db"
    handlePrivMsg <- Log.fileHandler privMsgLog Log.INFO >>= \lh ->
        return $ Log.setFormatter lh logFormatter
    Log.updateGlobalLogger "messages.PRIVMSG" (Log.addHandler handlePrivMsg)

    -- Setup stderr logger for main.
    handleMain <- Log.streamHandler stderr Log.INFO >>= \lh ->
        return $ Log.setFormatter lh logFormatter
    Log.updateGlobalLogger "main" (Log.addHandler handleMain)
    Log.updateGlobalLogger "main" (System.Log.Logger.setLevel Log.INFO)

    -- Setup stderr logger for monitoring.
    handleMonitor <- Log.streamHandler stderr Log.INFO >>= \lh ->
        return $ Log.setFormatter lh logFormatter
    Log.updateGlobalLogger "monitoring" (Log.addHandler handleMonitor)
    Log.updateGlobalLogger "monitoring" (System.Log.Logger.setLevel Log.INFO)

    -- Startup Bot.
    Log.infoM "main" $ unwords
        [ "Starting bot with configuration"
        , show conf
        , "and plugins"
        , show plugins
        , "and plugin arguments"
        , show args
        , "and data file locations"
        , show dfiles
        ]

    connect conf plugins args
  where
    logFormatter = Log.simpleLogFormatter "[$time : $loggername : $prio] $msg"

{- | Free resources used by the bot. Close files and connection to IRC
 - server. -}
tearDown :: BT.Bot
    -- ^ Bot to disconnect.
    -> IO ()
tearDown bot = do
    Log.infoM "main" "Going down"

    disconnect bot
    Log.removeAllHandlers

main :: IO ()
main = do
    arguments <- CMD.cmdArgs $ dikunt (showVersion version)
    configName <- getDataFileName "data/dikunt.config"
    config <- load [ Required configName ]
    config_executables <- require config "pathPlugins" :: IO [String]
    dataFileLocations <- dataFiles

    -- Extend $PATH with the extra locations.
    when (not . null $ pluginLocations arguments) $ do
        path <- getEnv "PATH"
        setEnv "PATH" (path ++ ":" ++ intercalate ":" (pluginLocations arguments))

    let pluginArguments = pluginArgs arguments
        executables = filter (\x -> not (x `elem` withoutPlugins arguments))
            (config_executables ++ withPlugins arguments)

    -- Start, loop and stop bot.
    case getBotConfig arguments of
        Just botConfig -> bracket (setup botConfig executables pluginArguments
            dataFileLocations) tearDown runBot
        -- TODO: Consider how to log this with hslogger.
        Nothing -> hPutStrLn stderr "Something wrong with configuration"

dataFiles :: IO [String]
dataFiles = mapM getDataFileName
    [ "data/InsultData.db"
    , "data/trump.txt"
    , "data/WordReplacerData.db"
    , "data/privmsgLog.db"
    , "data/msgLog.db"
    ]
