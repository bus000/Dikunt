module Main where

import qualified BotTypes as BT
import Data.ByteString.Char8 (unpack)
import Network.Download (openURI)
import Text.Regex.PCRE ((=~))
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Control.Monad (forever)
import Safe (readMay)

main :: IO ()
main = do
    [nick, _] <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- getLine
        output <- biblegem nick line

        case output of
            Just str -> putStrLn str
            Nothing -> return ()

biblegem :: String -> String -> IO (Maybe String)
biblegem nick s = case readMay s :: Maybe BT.Message of
    Just (BT.PrivMsg _ _ str) -> if str =~ pattern
        then getGem
        else return Nothing
    _ -> return Nothing
  where
    pattern = concat ["^", sp, nick, "\\:", ps, "biblegem", sp, "$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"

getGem :: IO (Maybe String)
getGem = do
    res <- openURI randomQuote
    case res of
        Left _ -> return Nothing
        Right passage -> return $ Just (format (unpack passage))

format :: String -> String
format passage = case passage =~ pattern :: [[String]] of
    [[_, verse, content]] -> verse ++ ": " ++ content
    _ -> passage
  where
    pattern = "^<b>(.*)<\\/b> (.*)$"

randomQuote :: String
randomQuote = "http://labs.bible.org/api/?passage=random"
