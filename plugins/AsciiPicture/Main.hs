module Main ( main ) where

import qualified BotTypes as BT
import Control.Monad (forever)
import qualified Data.Map as Map
import Safe (readMay)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import System.Process (readProcessWithExitCode)
import Text.Regex.PCRE ((=~))
import System.Environment (getArgs)

main :: IO ()
main = do
    [nick, _] <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- getLine
        handleMessage nick $ readMay line

handleMessage :: String -> Maybe BT.ServerMessage -> IO ()
handleMessage nick (Just (BT.ServerPrivMsg _ _ str))
    | str =~ helpPattern = help
    | [[_, url]] <- str =~ runPattern1 = asciipicture url
    | [[_, url]] <- str =~ runPattern2 = asciipicture url
    | [[_, url]] <- str =~ runPattern3 = asciipicture url
    | [[_, url]] <- str =~ runPattern4 = asciipicture url
  where
    helpPattern = concat ["^", sp, nick, ":", ps, "asciipicture", ps, "help",
        sp, "$"]
    runPattern1 = concat ["^", sp, "asciipicture:", ps, "(.*)$"]
    runPattern2 = concat ["^", sp, "asciiart:", ps, "(.*)$"]
    runPattern3 = concat ["^", sp, nick, ":", ps, "asciiart", ps, "(.*)$"]
    runPattern4 = concat ["^", sp, nick, ":", ps, "asciipicture", ps, "(.*)$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleMessage _ _ = return ()

asciipicture :: String -> IO ()
asciipicture picRef = case Map.lookup picRef buildIn of
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
    [ "dikunt: asciipicture help - show this message"
    , "asciipicture: url - show asciipicture of jpg linked to"
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
