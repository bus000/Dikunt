module Functions.AsciiPicture
  ( runAsciiPicture
  ) where

import Control.Monad.State
import System.Process
import System.Exit
import Data.List
import qualified BotTypes as BT

dickbut :: String
dickbut = "https://static1.fjcdn.com/thumbnails/comments/" ++
    "Dickbut+for+everybody+zentertainments+gets+a+dickbut+_" ++
    "fc88e4d586c873f470964fab580a9518.jpg"


runAsciiPicture :: BT.BotFunction
runAsciiPicture = do
    str <- BT.getValue BT.message
    asciiPicture str

asciiPicture :: String -> BT.Net (Maybe String)
asciiPicture str
    | "asciiart: dickbut" `isPrefixOf` str = doAsciiGeneration dickbut
    | "asciiart: http" `isPrefixOf` str =
        doAsciiGeneration $ drop (length "asciiart: ") str
    | otherwise = return Nothing
  where
    doAsciiGeneration url = do
        (e,s,_) <- liftIO $ readProcessWithExitCode "/usr/bin/jp2a"
            [url, "--size=40x40","--background=light"] []
        if e /= ExitSuccess
            then return Nothing
            else return $ Just s
