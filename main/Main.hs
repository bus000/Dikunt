{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified BotTypes as BT
import Bot (connect, disconnect, loop)
import Control.Exception (bracket)
import Data.Configurator (load, Worth(..), require)
import Paths_Dikunt
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
    } deriving (Data, Typeable, Show, Eq)

dikunt :: Dikunt
dikunt = Dikunt
    { server = "irc.freenode.org" &= help "Server to connect to"
    , nickname = "dikunt" &= help "Nick to use"
    , password = def &= help "Password to use"
    , channel = "#dikufags" &= help "Channel to connect to"
    , port = 6667 &= help "Port to connect to"
    } &=
        help "Bot to run on IRC channels" &=
        summary "Dikunt v0.0.1.0 (C) Magnus Stavngaard" &=
        helpArg [explicit, name "help", name "h"] &=
        versionArg [explicit, name "version", name "v"]

mode :: IO Dikunt
mode = cmdArgs dikunt

getBotConfig :: Dikunt -> BT.BotConfig
getBotConfig (Dikunt serv nick pass chan p) =
    BT.BotConfig serv nick pass chan p

main :: IO ()
main = do
    arguments <- mode
    configName <- getDataFileName "data/dikunt.config"
    config <- load [ Required configName ]
    executables <- require config "pathPlugins" :: IO [String]
    dataFileLocations <- dataFiles

    print dataFileLocations

    -- Start, loop and stop bot.
    bracket (connect (getBotConfig arguments) executables) disconnect loop

dataFiles :: IO [String]
dataFiles = mapM getDataFileName
    [ "data/InsultData.db"
    , "data/trump.txt"
    , "data/WordReplacerData.db"
    ]
