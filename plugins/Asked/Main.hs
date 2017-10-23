{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (foldM_)
import Data.Aeson (decode)
import qualified Data.Configurator as Conf
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Paths_Dikunt
import Prelude hiding (lines)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import System.Random (randomRs, newStdGen)
import Text.Regex.PCRE ((=~))
import qualified Types.BotTypes as BT

main :: IO ()
main = do
    (nick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    -- Load configuration.
    configName <- getDataFileName "data/dikunt.config"
    config <- Conf.load [ Conf.Required configName ]
    initialProb <- Conf.require config "asked-probability"

    randoms <- randomRs (0.0, 1.0) <$> newStdGen
    messages <- parseMessages <$> T.hGetContents stdin

    foldM_ (handleMessage nick) initialProb $ zip messages randoms

parseMessages :: T.Text -> [String]
parseMessages = mapMaybe getMessage . mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    getMessage (BT.ServerPrivMsg _ _ msg) = Just $ BT.getMessage msg
    getMessage _ = Nothing

handleMessage :: String -> Double -> (String, Double) -> IO Double
handleMessage nick threshold (str, r)
    | str =~ helpPattern = help nick >> return threshold
    | [[_, d]] <- str =~ changePattern = do
        putStrLn "Updated probability"
        return $ read d
    | r <= threshold = do
        putStrLn "Spurgt!"
        return threshold
    | otherwise = return threshold
  where
    helpPattern = concat ["^", sp, nick, ":", ps, "asked", ps, "help", sp]
    changePattern = concat ["^", sp, nick, ":", ps, "asked", ps, "set", ps,
        "probability", ps, "(0\\.[0-9]*|1.0|1|0)", sp, "$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"

help :: String -> IO ()
help nick = putStrLn $ unlines
    [ nick ++ ": asked help - Display this message"
    , nick ++ ": asked set probability <0-1> - Set probability to a number " ++
        "between 0 and 1"
    , "Otherwise prints \"Spurgt\" with the current probability to each message"
    ]
