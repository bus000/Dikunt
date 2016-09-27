module Functions.AsciiPicture
  ( runAsciiPicture
  ) where

import qualified BotTypes as BT
import Control.Monad.State
import Data.List
import System.Exit
import System.Process

dickbutt :: String
dickbutt = "https://static1.fjcdn.com/thumbnails/comments/" ++
    "Dickbut+for+everybody+zentertainments+gets+a+dickbut+_" ++
    "fc88e4d586c873f470964fab580a9518.jpg"

thumbsUp :: String
thumbsUp = "http://clipartix.com/wp-content/uploads/2016/04/Thumbs-up" ++
    "-clipart-cliparts-for-you.jpg"

pepe :: String
pepe = "https://ih1.redbubble.net/image.53530799.0943/flat,800x800,070,f.jpg"

justRight :: String
justRight = "http://static3.depositphotos.com/1001914/142/i/950/" ++
    "depositphotos_1429391-Hand-sign-ok.jpg"

runAsciiPicture :: BT.BotFunction
runAsciiPicture = do
    str <- BT.getValue BT.message
    asciiPicture str

asciiPicture :: String -> BT.Net (Maybe String)
asciiPicture str
    | "asciiart: dickbutt" `isPrefixOf` str = doAsciiGeneration dickbutt
    | "asciiart: (y)" `isPrefixOf` str = doAsciiGeneration thumbsUp
    | "asciiart: pepe" `isPrefixOf` str = doAsciiGeneration pepe
    | "asciiart: just right" `isPrefixOf` str = doAsciiGeneration justRight
    | "asciiart: http" `isPrefixOf` str =
        doAsciiGeneration $ drop (length "asciiart: ") str
    | otherwise = return Nothing
  where
    doAsciiGeneration url = do
        (e,s,_) <- liftIO $ readProcessWithExitCode "/usr/bin/jp2a"
            [url, "--size=40x30","--background=light"] []
        if e /= ExitSuccess
            then return Nothing
            else return $ Just s
