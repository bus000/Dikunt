module Functions.AsciiText
  ( runAsciiText
  ) where

import qualified BotTypes as BT
import Control.Monad.State
import Data.List
import System.Exit
import System.Process

runAsciiText :: BT.BotFunction
runAsciiText = do
    str <- BT.getValue BT.message
    asciiText str


asciiText :: String -> BT.Net (Maybe String)
asciiText str
    | "asciitext: " `isPrefixOf` str = do
        (e,s,_) <- liftIO $ readProcessWithExitCode "/usr/bin/toilet" [str'] []
        if e /= ExitSuccess
            then return Nothing
            else return $ Just s
    | otherwise = return Nothing
  where
    str' = drop (length "asciitext: ") str
