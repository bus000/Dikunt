module Main ( main ) where

import qualified BotTypes as BT
import Control.Monad (forever)
import qualified Data.Map as Map
import Safe (readMay)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import System.Process (readProcessWithExitCode)
import Text.Regex.PCRE ((=~))

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- getLine
        handleMessage (readMay line)

handleMessage :: Maybe BT.ServerMessage -> IO ()
handleMessage (Just (BT.ServerPrivMsg _ _ str))
    | str =~ helpPattern = help
    | [[_, url]] <- str =~ runPattern = asciiart url
  where
    helpPattern = concat ["^", sp, nick, ":", ps "asciiart", ps, "help", sp,
        "$"]
    runPattern = concat ["^", sp, "asciiart\\:", ps, "(.*)$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleMessage _ = return ()

asciiart :: String -> IO ()
asciiart picRef = case Map.lookup picRef buildIn of
    Just url -> generatePicture url
    Nothing -> generatePicture picRef

generatePicture :: String -> IO ()
generatePicture url = do
    (e, s, _) <- readProcessWithExitCode "/usr/bin/jp2a"
        [url, "--width=80", "--background=light"] []
    case e of
        ExitSuccess -> putStrLn s
        ExitFailure _ -> return ()

help :: IO ()
help = putStrLn $ unlines
    [ "asciiart: help - show this message"
    , "asciiart: url - show asciiart of jpg linked to"
    ]

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
