module Main ( main ) where

import qualified BotTypes as BT
import Control.Monad (forever)
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
        handleMessage (readMay line :: Maybe BT.ServerMessage)

handleMessage :: Maybe BT.ServerMessage -> IO ()
handleMessage (Just (BT.ServerPrivMsg _ _ str))
    | str =~ helpPattern = help
    | otherwise = case str =~ runPattern of
        [[_, text]] -> asciitext text
        _ -> return ()
  where
    helpPattern = concat ["^", sp, "asciitext\\:", ps, "help", sp]
    runPattern = concat ["^", sp, "asciitext\\:", ps, "(.*)$"]
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
