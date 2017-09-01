module Main (main) where

import Control.Error.Util (hush)
import Data.Aeson (decode)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Network.Download (openURI)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.Feed.Import (parseFeedString)
import Text.Feed.Query (feedItems, getItemTitle, getItemLink, getItemDescription)
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Types.BotTypes as BT

type User = String

data Source = BBC | SlashDot | DRAll | DRInternal | DRExternal
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
        "Source can be one of [BBC, /., DR, DR indland, DR udland]"

giveNews :: Source -> IO ()
giveNews src = do
    contents <- openURI $ sourceURL src
    case contents of
        Left _ -> putStrLn "Jeg kunne ikke hente nyheder"
        Right feed -> putStrLn $ fromMaybe "Jeg kunne ikke analysere feed"
            (analyseFeed $ (TS.unpack . TS.decodeUtf8) feed)

analyseFeed :: String -> Maybe String
analyseFeed feedStr = do
    feed <- parseFeedString feedStr
    item <- listToMaybe $ feedItems feed
    title <- getItemTitle item
    link <- getItemLink item
    description <- head . lines <$> getItemDescription item

    return $ title ++ "\n    " ++ description ++ "\n" ++ link

sourceURL :: Source -> String
sourceURL BBC = "http://feeds.bbci.co.uk/news/world/rss.xml"
sourceURL SlashDot = "http://rss.slashdot.org/Slashdot/slashdotMain"
sourceURL DRAll = "http://www.dr.dk/nyheder/service/feeds/allenyheder"
sourceURL DRInternal = "http://www.dr.dk/nyheder/service/feeds/indland"
sourceURL DRExternal = "http://www.dr.dk/nyheder/service/feeds/udland"

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
newsRequest = P.choice $ map P.try
    [ stringToken "BBC" *> return (GetNews BBC)
    , stringToken "/." *> return (GetNews SlashDot)
    , stringToken "DR" *> stringToken "indland" *> return (GetNews DRInternal)
    , stringToken "DR" *> stringToken "udland" *> return (GetNews DRExternal)
    , stringToken "DR" *> return (GetNews DRAll)
    , return (GetNews BBC)
    ]

token :: P.Parsec String () a -> P.Parsec String () a
token tok = P.spaces *> tok <* P.spaces

stringToken :: String -> P.Parsec String () String
stringToken str = token (P.string str)
