{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Types.BotTypes as BT
import System.Environment (getArgs)
import qualified Data.Char as Char
import Network.HTTP.Conduit (simpleHttp)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Data.Aeson (decode, FromJSON(..), (.:), withObject)
import Data.Maybe (mapMaybe)
import Control.Error (hush)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import Text.Parsec ((<|>))

type BotNick = String
type Chapter = Integer
type Verse = Integer
type Book = String
type URL = String

data Passage = Passage Book Chapter Verse
    | RandomPassage

newtype BibleQuote = BibleQuote String

instance FromJSON BibleQuote where
    parseJSON = withObject "BibleQuote" $ \o ->
        BibleQuote . convertUnicode <$> o .: "text"

data Request
    = HelpRequest BotNick
    | GemRequest Passage

main :: IO ()
main = do
    (botnick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    requests <- parseRequests botnick <$> T.hGetContents stdin

    mapM_ handleRequest requests

handleRequest :: Request -> IO ()
handleRequest (HelpRequest botnick) = performHelp botnick
handleRequest (GemRequest pass) = performGetGem pass

performHelp :: BotNick -> IO ()
performHelp botnick = do
    putStrLn $ botnick ++ ": biblegem help - Display this help message"
    putStrLn $ botnick ++ ": biblegem <book> <chapter>:<verse> - Display the "
        ++ "specific passage given"
    putStrLn $ botnick ++ ": biblegem - Display random bible quote"

performGetGem :: Passage -> IO ()
performGetGem pass = do
    passageString <- decode <$> simpleHttp (getURL pass)
    case passageString of
        Just (BibleQuote text:_) -> putStrLn text
        _ -> putStrLn "Parse error"

type RequestParser a = P.Parsec String () a

parseRequests :: BotNick -> T.Text -> [Request]
parseRequests botnick =
    mapMaybe getRequest . mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    getRequest (BT.ServerPrivMsg _ _ msg) =
        hush $ P.parse (request botnick) "" (BT.getMessage msg)
    getRequest _ = Nothing

request :: BotNick -> RequestParser Request
request botnick = stringToken (botnick ++ ":") *> stringToken "biblegem"
    *> P.choice requestTypes <* P.eof
  where
    requestTypes = map P.try
        [ helpRequest botnick
        , specificPassageRequest
        , randomPassageRequest
        ]

helpRequest :: BotNick -> RequestParser Request
helpRequest botnick = stringToken "help" *> return (HelpRequest botnick)

specificPassageRequest :: RequestParser Request
specificPassageRequest = GemRequest <$> (Passage <$> token book <*> chapter
    <* P.char ':' <*> token verse)
  where
    book = P.many1 (P.noneOf " ")
    chapter = P.nat
    verse = P.nat

randomPassageRequest :: RequestParser Request
randomPassageRequest = return $ GemRequest RandomPassage

token :: RequestParser a -> RequestParser a
token tok = P.spaces *> tok <* P.spaces

stringToken :: String -> RequestParser String
stringToken = token . P.string

getURL :: Passage -> URL
getURL (Passage book chapter verse) = "http://labs.bible.org/api/?passage="
    ++ book ++ "%20" ++ show chapter ++ ":" ++ show verse ++ "&type=json"
getURL RandomPassage = "http://labs.bible.org/api/?passage=random&type=json"

{- | Convert a text to a string and at the same time remove unicode characters
 - in decimal format. Meaning that substrings looking like, &#8211; are replaced
 - with their unicode character equivalent. -}
convertUnicode :: T.Text -> String
convertUnicode txt = fromEither (T.unpack txt) $ P.parse convertUnicode' "" txt
  where
    convertUnicode' = concat <$> P.many (P.try normalText <|> containsAnd) <* P.eof
    normalText = P.many1 (P.noneOf "&")
    containsAnd = P.try unicodeCharacter <|> (P.char '&' *> return "&")
    unicodeCharacter = (\x -> [Char.chr x]) <$>
        (P.string "&#" *> P.nat <* P.char ';')

fromEither :: b -> Either a b -> b
fromEither _ (Right x) = x
fromEither def (Left _) = def
