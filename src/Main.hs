{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import qualified Bot
import Control.Exception
import System.Console.CmdArgs

data Dikunt = Dikunt
  { server   :: String
  , username :: String
  , password :: String
  , channel  :: String
  } deriving (Data, Typeable, Show, Eq)

dikunt :: Dikunt
dikunt = Dikunt
  { server = def &= help "Server to connect to"
  , username = def &= help "Username to use"
  , password = def &= help "Password to use"
  , channel = def &= help "Channel to connect to"
  } &=
    help "Bot to run on IRC channels" &=
    summary "Dikunt v0.0.0.0 (C) Magnus Stavngaard"

mode :: IO Dikunt
mode = cmdArgs dikunt

main :: IO ()
main = do
    args <- mode
    let pass = password args

    bracket (Bot.connect pass) Bot.disconnect Bot.loop
