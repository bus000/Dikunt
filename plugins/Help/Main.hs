{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified BotTypes as BT
import Control.Applicative ((<$>))
import Control.Monad (forever)
import Data.Char (toLower)
import Data.Configurator (load, Worth(..), require)
import Paths_Dikunt
import Safe (readMay)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.Regex.PCRE ((=~))

main :: IO ()
main = do
    [nick, _] <- getArgs

    configName <- getDataFileName "data/dikunt.config"
    config <- load [ Required configName ]
    executables <- (map . map $ toLower) <$> require config "pathPlugins"

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- getLine
        handleInput nick executables $ readMay line

handleInput :: String -> [String] -> Maybe BT.ServerMessage -> IO ()
handleInput nick executables (Just (BT.ServerPrivMsg _ _ str))
    | str =~ helpPattern = help nick
    | str =~ runPattern = moduleHelp executables nick
    | otherwise = return ()
  where
    helpPattern = concat ["^", sp, nick, ":", ps, "help", ps, "help", sp, "$"]
    runPattern = concat ["^", sp, nick, ":", ps, "help", sp, "$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleInput _ _ _ = return ()

help :: String -> IO ()
help nick = putStrLn $ unlines
    [ nick ++ ": help help - Display this message"
    , nick ++ ": help - Display how to get help for other modules"
    ]

moduleHelp :: [String] -> String -> IO ()
moduleHelp executables nick = putStrLn . unlines $
    "Try one of the commands":map helpPlugin executables
  where
    helpPlugin pluginName = "    " ++ nick ++ ": help " ++ pluginName
