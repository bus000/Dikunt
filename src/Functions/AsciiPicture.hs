module Functions.AsciiPicture
  ( asciiPicture
  ) where

import qualified BotTypes as BT
import Control.Monad.State (liftIO)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)

asciiPicture :: BT.BotFunction
asciiPicture = BT.BotFunction
    { BT.shouldRun = shouldRun
    , BT.run = run
    , BT.help = "asciiart: <url> - Prints the jpg picture the URL points to"
    , BT.name = "AsciiArt"
    }

buildIn :: Map.Map String String
buildIn = Map.fromList
    [ ("dickbutt", "https://static1.fjcdn.com/thumbnails/comments/" ++
        "Dickbut+for+everybody+zentertainments+gets+a+dickbut+_" ++
        "fc88e4d586c873f470964fab580a9518.jpg")

    , ("(y)", "http://clipartix.com/wp-content/uploads/2016/04/Thumbs-up" ++
        "-clipart-cliparts-for-you.jpg")

    , ("pepe", "https://ih1.redbubble.net/image.53530799.0943/" ++
        "flat,800x800,070,f.jpg")

    , ("wewlad", "http://vignette1.wikia.nocookie.net/trollpasta/images/e/" ++
        "e6/Wew_lad.jpg")

    , ("just right", "http://static3.depositphotos.com/1001914/142/i/950/" ++
        "depositphotos_1429391-Hand-sign-ok.jpg")
    ]

shouldRun :: BT.Message -> BT.Net Bool
shouldRun (BT.PrivMsg _ _ msg) =
    let msg' = dropWhile isSpace msg
    in return $ "asciiart:" `isPrefixOf` msg'
shouldRun _ = return False

run :: BT.Message -> BT.Net [BT.Message]
run (BT.PrivMsg _ _ msg) =
    let clean = (unwords . drop 1 . words) msg
    in case Map.lookup clean buildIn of
        Just url -> generatePicture url
        Nothing -> generatePicture clean
run _ = fail "AsciiPicture only runs on PrivMsg's"

generatePicture :: String -> BT.Net [BT.Message]
generatePicture url = do
    (e, s, _) <- liftIO $ readProcessWithExitCode "/usr/bin/jp2a"
        [url, "--width=80", "--background=light"] []
    case e of
        ExitSuccess -> BT.privmsgs s
        ExitFailure _ -> BT.privmsgs "I do not understand yo URL"
