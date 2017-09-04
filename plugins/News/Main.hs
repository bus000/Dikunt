module Main (main) where

import Control.Error.Util (hush)
import Data.Aeson (decode)
import Data.List (intercalate)
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
type URL = String
type Key = [String]

data Source = Source URL Key deriving (Show, Read, Eq)

{- List of news sources with associated keys. When parsing requests the keys are
 - used to identify sources. The key ["word1", "word2"] will parse messages
 - looking like, "dikunt: news word1 word2". The ordering of the list matters.
 - If one key is a prefix of another it needs to be located later in the list.
 - -}
sources :: [Source]
sources =
    [ Source "http://feeds.bbci.co.uk/news/world/rss.xml" ["BBC"]
    , Source "http://rss.slashdot.org/Slashdot/slashdotMain" ["/."]
    , Source "http://www.dr.dk/nyheder/service/feeds/politik" ["DR", "politik"]
    , Source "http://www.dr.dk/nyheder/service/feeds/kultur" ["DR", "kultur"]
    , Source "http://www.dr.dk/nyheder/service/feeds/levnu" ["DR", "levnu"]
    , Source "http://www.dr.dk/nyheder/service/feeds/viden" ["DR", "viden"]
    , Source "http://www.dr.dk/nyheder/service/feeds/sporten" ["DR", "sporten"]
    , Source "http://www.dr.dk/nyheder/service/feeds/vejret" ["DR", "vejret"]
    , Source "http://www.dr.dk/nyheder/service/feeds/indland" ["DR", "indland"]
    , Source "http://www.dr.dk/nyheder/service/feeds/udland" ["DR", "udland"]
    , Source "http://www.dr.dk/nyheder/service/feeds/penge" ["DR", "penge"]
    , Source "http://www.dr.dk/nyheder/service/feeds/allenyheder" ["DR"]
    , Source "http://feeds.bbci.co.uk/news/world/rss.xml" [] -- Default source.
    ]

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
        "Source can be one of " ++ show sourceNames
  where
    sourceNames = map (\(Source _ key) -> unwords key) sources

giveNews :: Source -> IO ()
giveNews (Source url _) = do
    contents <- openURI url
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
newsRequest = P.choice $ map (P.try . parseSource) sources
  where
    parseSource source@(Source _ key) =
        foldr ((*>) . stringToken) (return $ GetNews source) key

token :: P.Parsec String () a -> P.Parsec String () a
token tok = P.spaces *> tok <* P.spaces

stringToken :: String -> P.Parsec String () String
stringToken str = token (P.string str)
