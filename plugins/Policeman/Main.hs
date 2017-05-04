module Main (main) where

import qualified BotTypes as BT
import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Random (sample)
import Data.Random.Extras (choice)
import Safe (readMay)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.Regex.PCRE ((=~))

main :: IO ()
main = do
    [nick, _] <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- getLine
        handleMessage nick (readMay line)

handleMessage :: String -> Maybe BT.ServerMessage -> IO ()
handleMessage nick (Just (BT.ServerPrivMsg _ _ str))
    | str =~ helpPattern = putStrLn $ help nick
    | containsCurse str = putStrLn =<< (sample $ choice responses)
  where
    helpPattern = concat ["^", sp, nick, ":", ps, "policeman", ps, "help", sp,
        "$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleMessage _ _ = return ()

containsCurse :: String -> Bool
containsCurse str = any (`elem` (wordsSpecial . map toLower $ str)) curseWords

help :: String -> String
help nick = unlines
    [ nick ++ ": policeman help - Display this help message."
    , "Otherwise - Prints warnings to swearing users."
    ]

responses :: [String]
responses =
    [ "Watch your mouth boy"
    , "Do you kiss your mother with that mouth?"
    ]

curseWords :: [String]
curseWords =
    [ "fucking"
    , "hell"
    , "bastard"
    , "fuck"
    , "idiot"
    , "crap"
    , "lort"
    , "goddamn"
    , "damn"
    , "shit"
    , "motherfucker"
    , "fucker"
    , "gay"
    ]

wordsSpecial :: String -> [String]
wordsSpecial = words . filter (`elem` normalStringLetters)
  where
    normalStringLetters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ']