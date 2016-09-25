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
  } deriving (Data, Typeable, Show, Eq)

dikunt :: Dikunt
dikunt = Dikunt
  { server = "irc.freenode.org" &= help "Server to connect to"
  , nickname = "dikunt" &= help "Nick to use"
  , password = def &= help "Password to use"
  , channel = "#dikufags" &= help "Channel to connect to"
  } &=
    help "Bot to run on IRC channels" &=
    summary "Dikunt v0.0.0.0 (C) Magnus Stavngaard"

mode :: IO Dikunt
mode = cmdArgs dikunt

main :: IO ()
main = do
    arguments <- mode
    let serv = server arguments
        pass = password arguments
        nick = nickname arguments
        chan = channel arguments

    bracket (Bot.connect serv chan nick pass) Bot.disconnect Bot.loop
