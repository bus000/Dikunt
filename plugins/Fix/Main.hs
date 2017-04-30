module Main (main) where

import qualified BotTypes as BT
import Control.Monad (forever)
import Control.Monad.State.Strict (runStateT, StateT, get, lift, put)
import Data.List.Utils (replace)
import Safe (readMay)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..), readFile)
import Text.Regex.PCRE ((=~))

main :: IO ()
main = do
    [nick, _] <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    _ <- runStateT (forever $ do
        line <- lift getLine
        handleInput nick $ readMay line) ""

    return ()

handleInput :: String -> Maybe BT.ServerMessage -> StateT String IO ()
handleInput nick (Just (BT.ServerPrivMsg _ _ str))
    | str =~ helpPattern = lift $ help nick
    | str =~ runPattern = case str =~ runPattern of
        [[_, old, new]] -> get >>= \m -> lift $ putStrLn (replace old new m)
        _ -> error "Should always match."
    | otherwise = put str
  where
    helpPattern = concat ["^", sp, "fix:", ps, "help", sp]
    runPattern = concat ["^", sp, "s/([a-zA-Z0-9]*)/([a-zA-Z0-9]*)", sp, "$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleInput _ _ = return ()

help :: String -> IO ()
help nick = putStrLn $ unlines
    [ nick ++ ": fix help - Display this message"
    , "s/<old>/<new> - Replace <old> with <new> in latest message"
    ]
