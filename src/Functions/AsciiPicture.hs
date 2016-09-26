module Functions.AsciiPicture
  ( runAsciiPicture,
  ) where

import Control.Monad.State
import System.Process
import System.Exit
import Data.List
import qualified BotTypes as BT

runAsciiPicture :: BT.BotFunction
runAsciiPicture = do
    str <- BT.getValue BT.message
    asciiPicture str

asciiPicture :: String -> BT.Net (Maybe String)
asciiPicture str
    | "asciiart: http" `isPrefixOf` str = do
        let url = drop (length "asciiart: ") str
        (e,s,_) <- liftIO $ readProcessWithExitCode "/usr/bin/jp2a"
            [url, "--size=25x10","--background=light"] []
        if e /= ExitSuccess
            then return Nothing
            else return $ Just s
    | otherwise = return Nothing
