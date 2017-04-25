module Main where

import qualified BotTypes as BT
import Data.Maybe (catMaybes)
import Safe (readMay)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.Regex.PCRE ((=~))

main :: IO ()
main = do
    [nick, _] <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    interact (unlines . catMaybes . map (danishMoan nick) . lines)

danishMoan :: String -> String -> Maybe String
danishMoan nick s = do
    message <- readMay s :: Maybe BT.ServerMessage
    case message of
        BT.ServerPrivMsg _ _ str -> getMoan nick str
        _ -> Nothing

getMoan :: String -> String -> Maybe String
getMoan nick str = case str =~ pattern of
    [[_, number]] -> do
        n <- readMay number :: Maybe Int
        if n < 100 && n > 0
        then return $ "Å" ++ replicate (n - 1) 'å' ++ "hhh"
        else Nothing
    _ -> Nothing
  where
    pattern = concat ["^", sp, nick, "\\:", ps, "danish", ps, "moan", ps,
        "([0-9]+)$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
