{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Prelude hiding (Word)
import Control.Applicative ((<$>), (<*>))
import Control.Error.Util (hush)
import Control.Monad (mapM, mapM_, unless, void)
import Control.Monad.State (StateT, liftIO, put, get, runStateT)
import Data.Aeson (decode)
import qualified Data.Char as Char
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple (NamedParam((:=)))
import Paths_Dikunt
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import System.Random (randomRs, newStdGen)
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Types.BotTypes as BT

{- | The type replacements are stored under in the database. -}
data Replacement = Replacement Int T.Text T.Text deriving (Show)

instance DB.FromRow Replacement where
    fromRow = Replacement <$> DB.field <*> DB.field <*> DB.field

instance DB.ToRow Replacement where
    toRow (Replacement id_ word replacement) = DB.toRow (id_, word, replacement)

type BotNick = String
type Word = String
type Probability = Double

{- | Lines from stdin are parsed to this structure which represents a request to
 - the plugin. -}
data Request
    = HelpRequest BotNick
    | AddReplacement BotNick Word String
    | SetProbability Probability
    | WordSequence [Word]

main :: IO ()
main = do
    (botnick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    randoms <- randomRs (0.0, 1.0) <$> newStdGen
    requests <- parseRequests botnick <$> T.hGetContents stdin

    dbFile <- getDataFileName "data/WordReplacerData.db"
    DB.withConnection dbFile (\c -> do
        initDatabase c
        handleRequests c (zip randoms requests))

{- | Handle a list of requests by performing the action requested. -}
handleRequests :: DB.Connection
    -- ^ Database connection.
    -> [(Probability, Request)]
    -- ^ Connections together with the probability of writing message.
    -> IO ()
handleRequests conn reqs = void $ runStateT (mapM_ (handleRequest conn) reqs) 0.1

handleRequest :: DB.Connection -> (Probability, Request) -> StateT Probability IO ()
handleRequest _ (_, HelpRequest botnick) =
    liftIO $ giveHelp botnick
handleRequest conn (_, AddReplacement botnick word withString) =
    liftIO $ addReplacement conn botnick word withString
handleRequest conn (_, SetProbability newProb) = do
    put newProb
    liftIO $ putStrLn "Sandsynlighed er sat!"
handleRequest conn (prob , WordSequence words) = do
    threshold <- get
    replacements <- liftIO $ getReplacements conn words
    case replacements of
        (Replacement _ w rep:_) | prob < threshold ->
            liftIO $ T.putStrLn
                (T.concat ["Jeg tror ikke du mener ", w, " men ", rep])
        _ -> return ()

addReplacement :: DB.Connection -> BotNick -> Word -> String -> IO ()
addReplacement conn botnick word replacement
    | (map Char.toLower botnick) == (map Char.toLower word) =
        putStrLn "Din naughty dreng"
    | otherwise = do
        insertReplacement conn word replacement
        putStrLn $ "Fra nu af ved jeg at " ++ word ++ " er det samme som "
            ++ replacement

{- | Print help message. -}
giveHelp :: BotNick
    -- ^ Nickname of Dikunt bot.
    -> IO ()
giveHelp nick = do
    putStrLn $ nick ++ ": wordreplacer add <word1>=<word2> - add word1=word2 "
        ++ "to database"
    putStrLn $ nick ++ ": wordreplacer set probability <d> - set the "
        ++ "probability of writing replacements"
    putStrLn $ nick ++  ": wordreplacer help - display this message"
    putStrLn "otherwise replaces words from database in messages"

parseRequests :: BotNick -> T.Text -> [Request]
parseRequests botnick =
    mapMaybe parseRequest . mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    parseRequest (BT.ServerPrivMsg _ _ msg) =
        hush $ P.parse (request botnick) "" (BT.getMessage msg)
    parseRequest _ = Nothing

type RequestParser a = P.Parsec String () a

request :: BotNick -> RequestParser Request
request botnick = P.choice requestTypes <* P.eof
  where
    requestTypes = map P.try
        [ helpRequest botnick
        , addReplacementRequest botnick
        , setProbabilityRequest botnick
        , wordListRequest
        ]

helpRequest :: BotNick -> RequestParser Request
helpRequest botnick = stringToken (botnick ++ ": ")
    *> stringToken "wordreplacer " *> stringToken "help"
    *> return (HelpRequest botnick)

addReplacementRequest :: BotNick -> RequestParser Request
addReplacementRequest botnick = do
    void $ stringToken (botnick ++ ": ")
    void $ stringToken "wordreplacer "
    void $ stringToken "add "
    word <- P.many1 (P.noneOf " \n\t=")
    replacement <- trim <$> (P.char '=' *> P.many1 P.anyChar)

    return $ AddReplacement botnick word replacement

setProbabilityRequest :: BotNick -> RequestParser Request
setProbabilityRequest botnick = do
    void $ stringToken (botnick ++ ": ")
    void $ stringToken "wordreplacer "
    void $ stringToken "set "
    void $ stringToken "probability "
    newprob <- P.floating

    if newprob >= 0.0 && newprob <= 1.0
        then return $ SetProbability newprob
        else P.unexpected "Probability should be between 0 and 1"

wordListRequest :: RequestParser Request
wordListRequest = WordSequence <$> token (word `P.sepBy` separator)
  where
    word = P.many1 $ P.noneOf " \n\t.:?!()[]{},"
    separator = P.many1 $ P.oneOf " \n\t.:?!()[]{},"

{- | Initialize a database by creating it if it does not exist and inserting
 - default mappings. -}
initDatabase :: DB.Connection
    -- ^ Connection to database to initialize.
    -> IO ()
initDatabase conn = do
    DB.execute_ conn "CREATE TABLE IF NOT EXISTS replacements \
        \(id INTEGER PRIMARY KEY, word TEXT UNIQUE, replacement TEXT)"

insertReplacement :: DB.Connection -> Word -> String -> IO ()
insertReplacement conn word replacement =
    DB.executeNamed conn "INSERT OR REPLACE INTO replacements \
        \(word, replacement) VALUES (:word, :replacement)"
        [":word" := map Char.toLower word, ":replacement" := replacement]

getReplacements :: DB.Connection -> [Word] -> IO [Replacement]
getReplacements conn words = concat <$> mapM getReplacement words
  where
    getReplacement word = DB.queryNamed conn "SELECT id, word, replacement \
        \FROM replacements WHERE word = :word"
            [":word" := word] :: IO [Replacement]

token :: RequestParser a -> RequestParser a
token tok = P.spaces *> tok <* P.spaces

stringToken :: String -> RequestParser String
stringToken = token . P.string

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile Char.isSpace
