module Main (main) where

import qualified Bot
import Control.Exception
import System.Environment

main :: IO ()
main = do
    (password:_) <- getArgs
    bracket (Bot.connect password) Bot.disconnect Bot.loop
