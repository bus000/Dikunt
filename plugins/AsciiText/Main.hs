module Main ( main ) where

import qualified BotTypes as BT
import Control.Monad (forever)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import System.Process (readProcessWithExitCode)
import Text.Regex.PCRE ((=~))
import Data.Aeson (decode)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

main :: IO ()
main = do
    [nick, _] <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- T.getLine
        handleMessage nick $ (decode . T.encodeUtf8) line

handleMessage :: String -> Maybe BT.ServerMessage -> IO ()
handleMessage nick (Just (BT.ServerPrivMsg _ _ str))
    | str =~ helpPattern = putStrLn $ help nick
    | [[_, text]] <- str =~ runPattern = asciitext text
  where
    helpPattern = concat ["^", sp, "asciitext:", ps, "help", sp]
    runPattern = concat ["^", sp, "asciitext:", ps, "(.*)$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleMessage _ _ = return ()

asciitext :: String -> IO ()
asciitext text = do
    (e, s, _) <- readProcessWithExitCode "/usr/bin/toilet" [text] []
    case e of
        ExitSuccess -> putStrLn s
        ExitFailure _ -> return ()

help :: String -> String
help nick = unlines
    [ nick ++ " asciitext help - display this message"
    , "asciitext: <text> - display text as asciitext"
    ]
