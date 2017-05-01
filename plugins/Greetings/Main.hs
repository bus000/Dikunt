module Main (main) where

import qualified BotTypes as BT
import System.Environment (getArgs)
import Control.Monad (forever)
import Safe (readMay)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.Regex.PCRE ((=~))

main :: IO ()
main = do
    [nick, _] <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- getLine
        handleMessage nick $ readMay line

handleMessage :: String -> Maybe BT.ServerMessage -> IO ()
handleMessage _ (Just (BT.ServerJoin (BT.IRCUser nick _ _) _)) =
    putStrLn $ "Welcome " ++ nick
handleMessage _ (Just (BT.ServerQuit (BT.IRCUser nick _ _) _)) =
    putStrLn $ "Goodbye " ++ nick
handleMessage _ (Just (BT.ServerPart (BT.IRCUser nick _ _) _ _)) =
    putStrLn $ "Goodbye " ++ nick
handleMessage nick (Just (BT.ServerPrivMsg _ _ str))
    | str =~ helpPattern = putStrLn $ help nick
    | otherwise = return ()
  where
    helpPattern = concat ["^", sp, nick, ":", ps, "greetings", ps, "help", sp,
        "$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleMessage _ _ = return ()

help :: String -> String
help nick = unlines
    [ nick ++ ": greetings help - Display this help message"
    , "otherwise shows greetings and goodbyes to people coming and going"
    ]
