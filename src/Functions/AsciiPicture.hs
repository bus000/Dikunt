module Functions.AsciiPicture
  ( runAsciiPicture,
  ) where

import System.Process
import System.Exit

runAsciiPicture :: String -> IO (Maybe String)
runAsciiPicture img = do
    (e,s,_) <- readProcessWithExitCode "/usr/bin/jp2a" [img, "--size=25x10","--background=light"] []
    --putStrLn s
    if e /= ExitSuccess
        then return Nothing
        else return $ Just s
