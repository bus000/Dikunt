module Functions.AsciiPicture
  ( runAsciiPicture,
  ) where

import System.Process
import System.Exit
import Data.List

runAsciiPicture :: String -> IO (Maybe String)
runAsciiPicture str
    | "asciiart: " `isPrefixOf` str = do
        let url = drop (length "asciiart: ") str
        (e,s,_) <- readProcessWithExitCode "/usr/bin/jp2a"
            [url, "--size=25x10","--background=light"] []
        if e /= ExitSuccess
            then return Nothing
            else return $ Just s
    | otherwise = return Nothing
