{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import qualified Bot
import Control.Exception
import System.Console.CmdArgs

data Dikunt = Dikunt
    { server   :: String
    , nickname :: String
    , password :: String
    , channel  :: String
    , port     :: Integer
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
        summary "Dikunt v0.0.0.0 (C) Magnus Stavngaard" &=
        helpArg [explicit, name "h"] &=
        versionArg [explicit, name "v"]

mode :: IO Dikunt
mode = cmdArgs dikunt

main :: IO ()
main = do
    arguments <- mode
    let serv = server arguments
        pass = password arguments
        nick = nickname arguments
        chan = channel arguments
        port' = port arguments

    bracket (Bot.connect serv chan nick pass port') Bot.disconnect Bot.loop
