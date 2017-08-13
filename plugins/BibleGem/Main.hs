module Main where

import qualified Types.BotTypes as BT
import Control.Monad (forever)
import System.Environment (getArgs)
import Network.HTTP.Conduit (simpleHttp)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.Regex.PCRE ((=~))
import Data.Aeson (decode)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

main :: IO ()
main = do
    (nick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- T.getLine
        handleInput nick $ (decode . T.encodeUtf8) line

handleInput :: String -> Maybe BT.ServerMessage -> IO ()
handleInput nick (Just (BT.ServerPrivMsg _ _ str))
    | str =~ helpPattern = putStrLn $ help nick
    | str =~ runPattern = getGem
  where
    helpPattern = concat ["^", sp, nick, ":", ps, "biblegem", ps, "help", sp,
        "$"]
    runPattern = concat ["^", sp, nick, ":", ps, "biblegem", sp, "$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleInput _ _ = return ()

getGem :: IO ()
getGem = do
    passage <- simpleHttp randomQuote
    putStrLn . format $ T.unpack (T.decodeUtf8 passage)

format :: String -> String
format passage = case passage =~ runPattern :: [[String]] of
    [[_, verse, content]] -> verse ++ ": " ++ content
    _ -> passage
  where
    runPattern :: String
    runPattern = "^<b>(.*)<\\/b> (.*)$"

help :: String -> String
help nick = unlines
    [ nick ++ ": biblegem help - Display this help message"
    , nick ++ ": biblegem - Display random bible quote"
    ]

randomQuote :: String
randomQuote = "http://labs.bible.org/api/?passage=random"
