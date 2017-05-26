module Main where

import qualified BotTypes as BT
import Control.Monad (forever)
import Data.Aeson (decode)
import Data.ByteString.Char8 (unpack)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Network.Download (openURI)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.Regex.PCRE ((=~))

main :: IO ()
main = do
    (nick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- T.getLine
        handleInput nick $ (decode . T.encodeUtf8) line

handleInput :: String -> Maybe BT.ServerMessage -> IO ()
handleInput nick (Just (BT.ServerPrivMsg _ _ text))
    | str =~ helpPattern = putStrLn $ help nick
    | str =~ runPattern = getGem
  where
    str = T.unpack text
    helpPattern = concat ["^", sp, nick, ":", ps, "biblegem", ps, "help", sp,
        "$"]
    runPattern = concat ["^", sp, nick, ":", ps, "biblegem", sp, "$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleInput _ _ = return ()

getGem :: IO ()
getGem = do
    res <- openURI randomQuote
    case res of
        Left _ -> return ()
        Right passage -> putStrLn (format (unpack passage))

{- TODO: Parse this as text as it contains unicode characters. -}
format :: String -> String
format passage = case passage =~ runPattern :: [[String]] of
    [[_, verse, content]] -> verse ++ ": " ++ content
    _ -> passage
  where
    runPattern = "^<b>(.*)<\\/b> (.*)$"

help :: String -> String
help nick = unlines
    [ nick ++ ": biblegem help - Display this help message"
    , nick ++ ": biblegem - Display random bible quote"
    ]

randomQuote :: String
randomQuote = "http://labs.bible.org/api/?passage=random"
