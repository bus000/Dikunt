module Main (main) where

import qualified Bot
import Control.Exception

main :: IO ()
main = bracket Bot.connect Bot.disconnect Bot.loop
