module Main (main) where

import qualified BotTypes as BT
import Control.Monad (forever)
import qualified Data.MarkovChain as MC
import Paths_Dikunt
import Safe (readMay)
import System.Environment (getArgs)
import System.IO (readFile)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import System.Random (newStdGen, StdGen)
import Text.Regex.PCRE ((=~))

main :: IO ()
main = do
    [nick, _] <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    trumpFile <- getDataFileName "data/trump.txt"
    trumpData <- readFile trumpFile

    forever $ do
        line <- getLine
        handleMessage (readMay line :: Maybe BT.ServerMessage) nick trumpData

handleMessage :: Maybe BT.ServerMessage -> String -> String -> IO ()
handleMessage (Just (BT.ServerPrivMsg BT.IRCUser{} _ msg)) nick trumpData
    | msg =~ helpPattern = putStrLn $ help nick
    | msg =~ runPattern = newStdGen >>= \r ->
        putStrLn (trumpQuote msg nick trumpData r)
    | otherwise = return ()
  where
    helpPattern = concat ["^", sp, nick, ":", ps, "trump", ps, "help", sp, "$"]
    runPattern = concat ["^", sp, nick, ":", ps, "trump", sp, "$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleMessage _ _ _ = return ()

help :: String -> String
help nick = unlines
    [ nick ++ ": trump help - Display this help message."
    , nick ++ ": trump - Output a markov random trump quote."
    ]

trumpQuote :: String -> String -> String -> StdGen -> String
trumpQuote msg nick trumpData gen =
    "Friends, delegates and fellow Americans: " ++ sentence
  where
    ws = words trumpData
    trumpText = MC.run 2 ws 0 gen
    trumpText' = drop 1 $ dropWhile (notElem '.') trumpText
    sentence = takeWhile ('.' /=) (unwords trumpText') ++ "."

{-trump :: BT.BotFunction-}
{-trump = BT.BotFunction-}
    {-{ BT.shouldRun = shouldRun-}
    {-, BT.run = run-}
    {-, BT.help = "<nick>: trump - Outputs a markov random trump quote."-}
    {-, BT.name = "Trump"-}
    {-}-}

{-shouldRun :: BT.Message -> BT.Net Bool-}
{-shouldRun (BT.PrivMsg _ _ msg) = do-}
    {-nick <- BT.getValue BT.nickname-}
    {-case words msg of-}
        {-[first, second] ->-}
            {-return $ (first == nick ++ ":") && (second == "trump")-}
        {-_ -> return False-}
{-shouldRun _ = return False-}

{-run :: BT.Message -> BT.Net [BT.Message]-}
{-run BT.PrivMsg{} = do-}
    {-gen <- liftIO Random.newStdGen-}
    {-BT.privmsgs =<< (liftIO $ runTrump gen)-}
{-run _ = fail "Trump should only run on PrivMsg's."-}

{-runTrump :: Random.StdGen -> IO String-}
{-runTrump gen = do-}
    {-content <- readFile trumpFile-}
    {-let ws = words content-}
        {-trumpText = MC.run 2 ws 0 gen-}
        {-trumpText' = drop 1 $ dropWhile (notElem '.') trumpText-}
        {-sentence = takeWhile ('.' /=) (unwords trumpText') ++ "."-}

    {-return $ "Friends, delegates and fellow Americans: " ++ sentence-}

{-trumpFile :: FilePath-}
{-trumpFile = "./data/trump.txt"-}
