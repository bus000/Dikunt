module Main (main) where

import Control.Error.Util (note, hush)
import Data.Aeson (decode)
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Network.Download (openAsFeed)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.Feed.Query (feedItems, getItemTitle, getItemLink, getItemDescription)
import Text.Feed.Types (Item)
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Types.BotTypes as BT

type User = String

data Source = BBC | SlashDot
  deriving (Show, Read, Eq)

data Request
    = Help User
    | GetNews Source
  deriving (Show, Read, Eq)

main :: IO ()
main = do
    (nick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    messages <- parseMessages nick <$> T.hGetContents stdin

    mapM_ handleMessage messages

handleMessage :: Request -> IO ()
handleMessage (Help nick) = giveHelp nick
handleMessage (GetNews src) = giveNews src

giveHelp :: String -> IO ()
giveHelp nick = do
    putStrLn $ nick ++ ": news help - Display this help message"
    putStrLn $ nick ++ ": news - Display news from default source"
    putStrLn $ nick ++ ": news <source> - Display news from given source. " ++
        "Source can be one of [BBC, /.]"

giveNews :: Source -> IO ()
giveNews src = do
    feed <- openAsFeed $ sourceURL src
    either putStrLn putStrLn (giveNews' feed)
  where
    giveNews' f = do -- % TODO: refactor.
        f' <- f
        note "Feed analysis failed" $ analyseFeed (feedItems f')

analyseFeed :: [Item] -> Maybe String
analyseFeed [] = Nothing
analyseFeed (item:_) = do
    title <- getItemTitle item
    link <- getItemLink item
    description <- head . lines <$> getItemDescription item

    return $ title ++ "\n    " ++ description ++ "\n" ++ link

sourceURL :: Source -> String
sourceURL BBC = "http://feeds.bbci.co.uk/news/world/rss.xml"
sourceURL SlashDot = "http://rss.slashdot.org/Slashdot/slashdotMain"

parseMessages :: User -> T.Text -> [Request]
parseMessages botnick =
    mapMaybe getRequest . mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    getRequest (BT.ServerPrivMsg _ _ msg) =
        hush $ P.parse (request botnick) "(stdin)" (BT.getMessage msg)
    getRequest _ = Nothing

request :: User -> P.Parsec String () Request
request nick = token (P.string $ nick ++ ": news") *>
    (P.try (helpRequest nick) <|> P.try newsRequest) <* P.eof

helpRequest :: User -> P.Parsec String () Request
helpRequest nick = token (P.string "help") *> return (Help nick)

newsRequest :: P.Parsec String () Request
newsRequest = P.try bbcSource <|> P.try slashDotSource <|> return (GetNews BBC)
  where
    bbcSource = token (P.string "BBC") *> return (GetNews BBC)
    slashDotSource = token (P.string "/.") *> return (GetNews SlashDot)

token :: P.Parsec String () a -> P.Parsec String () a
token tok = P.spaces *> tok <* P.spaces
