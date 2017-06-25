module Main (main) where

import qualified BotTypes as BT
import Control.Monad (foldM_)
import Data.Aeson (decode)
import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Prelude hiding (lines)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import System.Random (randomRs, newStdGen)
import Text.Regex.PCRE ((=~))

main :: IO ()
main = do
    (nick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    randoms <- randomRs (0.0, 1.0) <$> newStdGen
    messages <- parseMessages <$> T.hGetContents stdin

    foldM_ (handleMessage nick) (randoms, 0.01) messages

parseMessages :: T.Text -> [String]
parseMessages = catMaybes . map getMessage . catMaybes .
    map (decode . T.encodeUtf8) . T.lines
  where
    getMessage (BT.ServerPrivMsg _ _ str) = Just str
    getMessage _ = Nothing

handleMessage :: BT.Nickname -> ([Double], Double) -> String -> IO ([Double], Double)
handleMessage nick (r:rs, threshold) str
    | str =~ helpPattern = help nick >> return (rs, threshold)
    | [[_, d]] <- str =~ changePattern = do
        putStrLn "Updated probability"
        return (rs, read d)
    | r <= threshold = do
        putStrLn "Spurgt!"
        return (rs, threshold)
    | otherwise = do
        return (rs, threshold)
  where
    helpPattern = concat ["^", sp, nick, ":", ps, "asked", ps, "help", sp]
    changePattern = concat ["^", sp, nick, ":", ps, "asked", ps, "set", ps,
        "probability", ps, "(0\\.[0-9]*|1.0|1|0)", sp, "$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleMessage _ ([], _) _ = error "Should be given an infinite list"

help :: BT.Nickname -> IO ()
help nick = putStrLn $ unlines
    [ nick ++ ": asked help - Display this message"
    , nick ++ ": asked set probability <0-1> - Set probability to a number " ++
        "between 0 and 1"
    , "Otherwise prints \"Spurgt\" with the current probability to each message"
    ]
