module Main (main) where

import qualified Types.BotTypes as BT
import Control.Monad (forever)
import qualified Data.MarkovChain as MC
import Paths_Dikunt
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..), readFile)
import System.Random (newStdGen, StdGen)
import Text.Regex.PCRE ((=~))
import Data.Aeson (decode)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

main :: IO ()
main = do
    (nick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    trumpFile <- getDataFileName "data/trump.txt"
    trumpData <- readFile trumpFile

    forever $ do
        line <- T.getLine
        handleMessage nick trumpData $ (decode . T.encodeUtf8) line

handleMessage :: String -> String -> Maybe BT.ServerMessage -> IO ()
handleMessage nick trumpData (Just (BT.ServerPrivMsg BT.IRCUser{} _ msg))
    | msg =~ helpPattern = putStrLn $ help nick
    | msg =~ runPattern = newStdGen >>= \r -> putStrLn (trumpQuote trumpData r)
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

trumpQuote :: String -> StdGen -> String
trumpQuote trumpData gen =
    "Friends, delegates and fellow Americans: " ++ sentence
  where
    ws = words trumpData
    trumpText = MC.run 2 ws 0 gen
    trumpText' = drop 1 $ dropWhile (notElem '.') trumpText
    sentence = takeWhile ('.' /=) (unwords trumpText') ++ "."
