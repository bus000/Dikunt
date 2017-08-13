module Main (main) where

import qualified Types.BotTypes as BT
import Control.Error.Util (note)
import Control.Monad (forever)
import Network.Download (openAsFeed)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.Feed.Query (feedItems, getItemTitle, getItemLink, getItemDescription)
import Text.Feed.Types (Item)
import Text.Regex.PCRE ((=~))
import Data.Aeson (decode)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

main :: IO ()
main = do
    (nick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    forever $ do
        line <- T.getLine
        handleMessage nick $ (decode . T.encodeUtf8) line

handleMessage :: String -> Maybe BT.ServerMessage -> IO ()
handleMessage nick (Just (BT.ServerPrivMsg BT.IRCUser{} _ msg))
    | msg =~ helpPattern = help nick
    | msg =~ runPattern = giveNews
  where
    helpPattern = concat ["^", sp, nick, ":", ps, "news", ps, "help", sp, "$"]
    runPattern = concat ["^", sp, nick, ":", ps, "news", sp, "$"]
    sp = "[ \\t]*"
    ps = "[ \\t]+"
handleMessage _ _ = return ()

help :: String -> IO ()
help nick = do
    putStrLn $ nick ++ ": news help - Display this help message"
    putStrLn $ nick ++ ": news - Display news"

giveNews :: IO ()
giveNews = do
    feed <- openAsFeed rssFeed
    either putStrLn putStrLn (giveNews' feed)
  where
    giveNews' f = do
        f' <- f
        note "Feed analysis failed" $ analyseFeed (feedItems f')

analyseFeed :: [Item] -> Maybe String
analyseFeed [] = Nothing
analyseFeed (item:_) = do
    title <- getItemTitle item
    link <- getItemLink item
    description <- getItemDescription item

    return $ title ++ "\n    " ++ description ++ "\n" ++ link

rssFeed :: String
rssFeed = "http://feeds.bbci.co.uk/news/world/rss.xml"
