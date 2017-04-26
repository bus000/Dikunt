module Main (main) where

import qualified BotTypes as BT
import Control.Monad (forever)
import qualified Data.MarkovChain as MC
import Paths_Dikunt
import Safe (readMay)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..), readFile)
import System.Random (newStdGen, StdGen)
import Text.Regex.PCRE ((=~))

main :: IO ()
main = do
    [nick, _] <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    trumpFile <- getDataFileName "data/trump.txt"
    trumpData <- readFile trumpFile

    forever $ do
        line <- getLine
        handleMessage (readMay line :: Maybe BT.ServerMessage) nick trumpData

handleMessage :: Maybe BT.ServerMessage -> String -> String -> IO ()
handleMessage (Just (BT.ServerPrivMsg BT.IRCUser{} _ msg)) nick trumpData
    | msg =~ helpPattern = putStrLn $ help nick
    | msg =~ runPattern = newStdGen >>= \r ->
        putStrLn (trumpQuote msg nick trumpData r)
    | otherwise = return ()
  where
    helpPattern = concat ["^", sp, nick, ":", ps, "trump", ps, "help", sp, "$"]
    runPattern = concat ["^", sp, nick, ":", ps, "trump", sp, "$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleMessage _ _ _ = return ()

help :: String -> String
help nick = unlines
    [ nick ++ ": trump help - Display this help message."
    , nick ++ ": trump - Output a markov random trump quote."
    ]

trumpQuote :: String -> String -> String -> StdGen -> String
trumpQuote msg nick trumpData gen =
    "Friends, delegates and fellow Americans: " ++ sentence
  where
    ws = words trumpData
    trumpText = MC.run 2 ws 0 gen
    trumpText' = drop 1 $ dropWhile (notElem '.') trumpText
    sentence = takeWhile ('.' /=) (unwords trumpText') ++ "."
