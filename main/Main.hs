{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Map.Strict as Map
import qualified System.Log.Logger
import System.IO (stderr)
import qualified System.Log.Logger as Log
import qualified System.Log.Handler as Log
import qualified System.Log.Handler.Simple as Log
import qualified System.Log.Formatter as Log
import Bot (connect, disconnect, loop)
import qualified BotTypes as BT
import Control.Exception (bracket)
import Data.Configurator (load, Worth(..), require)
import Data.Version (showVersion)
import Paths_Dikunt (getDataFileName, version)
import System.Console.CmdArgs
    ( Data
    , Typeable
    , helpArg
    , explicit
    , name
    , (&=)
    , help
    , versionArg
    , def
    , summary
    , cmdArgs
    )

type DataFiles = Map.Map String String

data Dikunt = Dikunt
    { server     :: String
    , nickname   :: String
    , password   :: String
    , channel    :: String
    , port       :: Integer
    , pluginArgs :: [String]
    } deriving (Data, Typeable, Show, Eq)

dikunt :: String -> Dikunt
dikunt vers = Dikunt
    { server = "irc.freenode.org" &= help "Server to connect to"
    , nickname = "dikunt" &= help "Nick to use"
    , password = def &= help "Password to use"
    , channel = "#dikufags" &= help "Channel to connect to"
    , port = 6667 &= help "Port to connect to"
    , pluginArgs = [] &= explicit &= name "plugin-arg" &=
        help "Argument that is passed to all plugins started by Dikunt."
    } &=
        help "Bot to run on IRC channels" &=
        summary ("Dikunt v" ++ vers ++ " (C) Magnus Stavngaard") &=
        helpArg [explicit, name "help", name "h"] &=
        versionArg [explicit, name "version", name "v"]

getBotConfig :: Dikunt -> BT.BotConfig
getBotConfig (Dikunt serv nick pass chan p _) =
    BT.BotConfig serv nick pass chan p

{- | Allocate resources used by the bot. Setup logging and connection to IRC
 - server. -}
setup :: BT.BotConfig
    -- ^ Configuration of bot.
    -> [FilePath]
    -- ^ List of plugins to startup.
    -> [String]
    -- ^ List of arguments to plugins.
    -> DataFiles
    -- ^ Data files the program uses.
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
    arguments <- cmdArgs $ dikunt (showVersion version)
    configName <- getDataFileName "data/dikunt.config"
    config <- load [ Required configName ]
    executables <- require config "pathPlugins" :: IO [String]
    dataFileLocations <- dataFiles

    let botConfig = getBotConfig arguments
        pluginArguments = getArguments dataFileLocations ++ pluginArgs arguments

    -- Start, loop and stop bot.
    bracket (setup botConfig executables pluginArguments dataFileLocations)
        tearDown loop

dataFiles :: IO DataFiles
dataFiles = do
    datafileNames <- mapM getDataFileName paths
    return $ Map.fromList (zip paths datafileNames)
  where
    paths = [ "data/InsultData.db"
        , "data/trump.txt"
        , "data/WordReplacerData.db"
        , "data/privmsgLog.db"
        , "data/msgLog.db"
        ]

getArguments :: DataFiles -> [String]
getArguments = map getArgument . Map.toList
  where
    getArgument (key, value) = "--" ++ key ++ "=" ++ value
