module Main ( main ) where

import Text.Regex.PCRE ((=~))
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Control.Monad (forever)
import Safe (readMay)
import qualified BotTypes as BT
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- getLine
        output <- handleMessage (readMay line :: Maybe BT.Message)
        case output of
            Just str -> putStrLn str
            Nothing -> return ()

handleMessage :: Maybe BT.Message -> IO (Maybe String)
handleMessage (Just (BT.PrivMsg _ _ str))
    | str =~ helpPattern = help
    | otherwise = case str =~ pattern of
        [[_, url]] -> asciiart url
        _ -> return Nothing
  where
    helpPattern = concat ["^", sp, "asciiart\\:", ps, "help", sp]
    pattern = concat ["^", sp, "asciiart\\:", ps, "(.*)$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleMessage _ = return Nothing

asciiart :: String -> IO (Maybe String)
asciiart picRef = case Map.lookup picRef buildIn of
    Just url -> generatePicture url
    Nothing -> generatePicture picRef

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

generatePicture :: String -> IO (Maybe String)
generatePicture url = do
    (e, s, _) <- readProcessWithExitCode "/usr/bin/jp2a"
        [url, "--width=80", "--background=light"] []
    case e of
        ExitSuccess -> return $ Just s
        ExitFailure _ -> return Nothing

help :: IO (Maybe String)
help = return $ Just (unlines
    [ "asciiart: help - show this message"
    , "asciiart: url - show asciiart of jpg linked to"
    ])
