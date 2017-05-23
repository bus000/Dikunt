{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

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

main :: IO ()
main = do
    arguments <- cmdArgs $ dikunt (showVersion version)
    configName <- getDataFileName "data/dikunt.config"
    config <- load [ Required configName ]
    executables <- require config "pathPlugins" :: IO [String]
    dataFileLocations <- dataFiles

    -- Print data file locations so they can be found.
    print dataFileLocations

    let botConfig = getBotConfig arguments
        pluginArguments = pluginArgs arguments

    -- Start, loop and stop bot.
    bracket (connect botConfig executables pluginArguments) disconnect loop

dataFiles :: IO [String]
dataFiles = mapM getDataFileName
    [ "data/InsultData.db"
    , "data/trump.txt"
    , "data/WordReplacerData.db"
    ]
