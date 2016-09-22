module Main (main) where

import qualified Bot
import Control.Exception
import Control.Monad
import Network
import System.IO

main :: IO ()
main = bracket Bot.connect Bot.disconnect Bot.loop
