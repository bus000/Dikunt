module Main where

import qualified BotTypes as BT
import Safe (readMay)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.Regex.PCRE ((=~))
import Control.Monad (forever)

main :: IO ()
main = do
    [nick, _] <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- getLine
        handleInput nick $ readMay line

handleInput :: String -> Maybe (BT.ServerMessage) -> IO ()
handleInput nick (Just (BT.ServerPrivMsg _ _ str))
    | str =~ helpPattern = putStrLn $ help nick
    | [[_, n]] <- str =~ runPattern = putStr $ getMoan (readMay n)
  where
    helpPattern = concat ["^", sp, nick, ":", ps, "danishmoan", ps, "help", sp,
        "$"]
    runPattern = concat ["^", sp, nick, ":", ps, "danishmoan", ps, "([0-9]+)",
        sp, "$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleInput _ _ = return ()

help :: String -> String
help nick = unlines
    [ nick ++ ": danishmoan help - Display this help message."
    , nick ++ ": danishmoan <n> - Print a moan using <n> å's."
    ]

getMoan :: Maybe Int -> String
getMoan (Just n) = "Å" ++ replicate (n - 1) 'å' ++ "hhh\n"
getMoan _ = ""
