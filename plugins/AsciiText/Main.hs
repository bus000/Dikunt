module Main ( main ) where

import qualified BotTypes as BT
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.Regex.PCRE ((=~))
import Safe (readMay)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Control.Monad (forever)
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- getLine
        handleMessage (readMay line :: Maybe BT.Message)

handleMessage :: Maybe BT.Message -> IO ()
handleMessage (Just (BT.PrivMsg _ _ str))
    | str =~ helpPattern = help
    | otherwise = case str =~ pattern of
        [[_, text]] -> asciitext text
        _ -> return ()
  where
    helpPattern = concat ["^", sp, "asciitext\\:", ps, "help", sp]
    pattern = concat ["^", sp, "asciitext\\:", ps, "(.*)$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleMessage _ = return ()

asciitext :: String -> IO ()
asciitext text = do
    (e, s, _) <- readProcessWithExitCode "/usr/bin/toilet" [text] []
    case e of
        ExitSuccess -> putStrLn s
        ExitFailure _ -> return ()

help :: IO ()
help = putStrLn $ unlines
    [ "asciitext: help - display this message"
    , "asciitext: <text> - display text as asciitext"
    ]
