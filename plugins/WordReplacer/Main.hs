{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Control.Error.Util (hush)
import Control.Monad (void, when)
import Control.Monad.State (StateT, liftIO, get, runStateT, modify)
import Data.Aeson (decode)
import qualified Data.Char as Char
import qualified Data.Configurator as Conf
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple (NamedParam((:=)))
import Paths_Dikunt
import Prelude hiding (Word, words)
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

    -- Load configuration.
    configName <- getDataFileName "data/dikunt.config"
    config <- Conf.load [ Conf.Required configName ]
    replacerProb <- Conf.require config "wordreplacer-probability"

    randoms <- randomRs (0.0, 1.0) <$> newStdGen
    requests <- parseRequests botnick <$> T.hGetContents stdin

    dbFile <- getDataFileName "data/WordReplacerData.db"
    DB.withConnection dbFile (\c -> do
        initDatabase c
        handleRequests c replacerProb (zip randoms requests))

{- | Handle a list of requests by performing the action requested. -}
handleRequests :: DB.Connection
    -- ^ Database connection.
    -> Probability
    -- ^ Initial probability of replacing.
    -> [(Probability, Request)]
    -- ^ Connections together with the probability of writing message.
    -> IO ()
handleRequests conn initProb reqs =
    void $ runStateT (mapM_ handleRequest reqs) initialState
  where
    initialState = (initProb, conn)

{- | State we run the wordreplacer in. The state contains a database connection
 - and the current probability. Words are only replaced if a generated number is
 - lower than the current probability. -}
type WordReplacerState a = StateT (Probability, DB.Connection) IO a

{- | Handle a single request. -}
handleRequest :: (Probability, Request)
    -- ^ The request and the probability that the request should be served.
    -> WordReplacerState ()
handleRequest (_, HelpRequest botnick) =
    handleHelp botnick
handleRequest (_, AddReplacement botnick word withString) =
    handleAdd botnick word withString
handleRequest (_, SetProbability newProb) =
    handleSetProbability newProb
handleRequest (prob , WordSequence words) =
    handleWordReplacement words prob

{- | Add a replacement to the database if the word is not the nickname of the
 - bot. -}
handleAdd :: BotNick
    -- ^ Nickname of bot.
    -> Word
    -- ^ Word to replace.
    -> String
    -- ^ String to replace with.
    -> WordReplacerState ()
handleAdd botnick word replacement
    | map Char.toLower botnick == map Char.toLower word =
        liftIO $ putStrLn "Din naughty dreng"
    | otherwise = do
        conn <- getConnection
        liftIO $ insertReplacement conn word replacement
        liftIO $ putStrLn ("Fra nu af ved jeg at " ++ word ++ " er det samme som "
            ++ replacement)

{- | Print help message. -}
handleHelp :: BotNick
    -- ^ Nickname of Dikunt bot.
    -> WordReplacerState ()
handleHelp nick = liftIO $ sequence_
    [ putStrLn $ nick ++ ": wordreplacer add <word1>=<word2> - add word1=word2 "
        ++ "to database"
    , putStrLn $ nick ++ ": wordreplacer set probability <d> - set the "
        ++ "probability of writing replacements"
    , putStrLn $ nick ++  ": wordreplacer help - display this message"
    , putStrLn "otherwise replaces words from database in messages"
    ]

{- | Set the current probability in the state. -}
handleSetProbability :: Probability
    -- ^ The new probability.
    -> WordReplacerState ()
handleSetProbability newprob = do
    setProbability newprob
    liftIO $ putStrLn "Sandsynlighed er sat!"

{- | Look up each word in the database and find replacements. -}
handleWordReplacement :: [Word]
    -- ^ Words to find and output replacements for.
    -> Probability
    -- ^ The probability of printing the replacements.
    -> WordReplacerState ()
handleWordReplacement words prob = do
    threshold <- getCurrentProbability
    conn <- getConnection
    replacements <- liftIO $ getReplacements conn words

    when ((not . null) replacements && prob < threshold) $
        liftIO (T.putStrLn $ replacementString (head replacements))
  where
    replacementString (Replacement _ w rep) = T.concat
        ["Jeg tror ikke du mener " , w , " men " , rep]

{- | Parse requests to the plugin from a text string. A new potential request
 - is assumed to be on each line. -}
parseRequests :: BotNick
    -- ^ The nickname of the bot.
    -> T.Text
    -- ^ The text to parse requests from.
    -> [Request]
parseRequests botnick =
    mapMaybe parseRequest . mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    parseRequest (BT.ServerPrivMsg _ _ msg) =
        hush $ P.parse (request botnick) "" (BT.getMessage msg)
    parseRequest _ = Nothing

{- | Type to use when parsing requests. -}
type RequestParser a = P.Parsec String () a

{- | Parse a request from a line of text. A request is one of HelpRequest,
 - AddReplacement, SetProbability or WordSequence. -}
request :: BotNick
    -- ^ The nickname of the bot.
    -> RequestParser Request
request botnick = P.choice requestTypes <* P.eof
  where
    requestTypes = map P.try
        [ helpRequest botnick
        , addReplacementRequest botnick
        , setProbabilityRequest botnick
        , wordListRequest
        ]

{- | Parse a help request. -}
helpRequest :: BotNick
    -- ^ Nickname of the bot.
    -> RequestParser Request
helpRequest botnick = stringToken (botnick ++ ": ")
    *> stringToken "wordreplacer " *> stringToken "help"
    *> return (HelpRequest botnick)

{- | Parse a request to add a replacement. -}
addReplacementRequest :: BotNick
    -- ^ Nickname of the bot.
    -> RequestParser Request
addReplacementRequest botnick = do
    void $ stringToken (botnick ++ ": ")
    void $ stringToken "wordreplacer "
    void $ stringToken "add "
    word <- P.many1 (P.noneOf " \n\t=")
    replacement <- trim <$> (P.char '=' *> P.many1 P.anyChar)

    return $ AddReplacement botnick word replacement

{- | Parse a request to set the current probability. -}
setProbabilityRequest :: BotNick
    -- ^ Nickname of the bot.
    -> RequestParser Request
setProbabilityRequest botnick = do
    void $ stringToken (botnick ++ ": ")
    void $ stringToken "wordreplacer "
    void $ stringToken "set "
    void $ stringToken "probability "
    newprob <- P.floating

    if newprob >= 0.0 && newprob <= 1.0
        then return $ SetProbability newprob
        else P.unexpected "Probability should be between 0 and 1"

{- | Parses anything to a list of words separated by spaces and special
 - characters. -}
wordListRequest :: RequestParser Request
wordListRequest = WordSequence <$> token (word `P.sepBy` separator)
  where
    word = P.many1 $ P.noneOf " \n\t.:?!()[]{},"
    separator = P.many1 $ P.oneOf " \n\t.:?!()[]{},"

{- | Initialize a database by creating it if it does not exist. -}
initDatabase :: DB.Connection
    -- ^ Connection to database to initialize.
    -> IO ()
initDatabase conn =
    DB.execute_ conn "CREATE TABLE IF NOT EXISTS replacements \
        \(id INTEGER PRIMARY KEY, word TEXT UNIQUE, replacement TEXT)"

{- | Insert or replace a replacement in the database of replacements. -}
insertReplacement :: DB.Connection
    -- ^ Database connection.
    -> Word
    -- ^ Word to replace.
    -> String
    -- ^ String to replace with.
    -> IO ()
insertReplacement conn word replacement =
    DB.executeNamed conn "INSERT OR REPLACE INTO replacements \
        \(word, replacement) VALUES (:word, :replacement)"
        [":word" := map Char.toLower word, ":replacement" := replacement]

{- | Get list of all replacements for words given. The function looks up each
 - word in the database and concatenates the result. -}
getReplacements :: DB.Connection
    -- ^ Database connection.
    -> [Word]
    -- ^ List of words to find replacements for.
    -> IO [Replacement]
getReplacements conn words = concat <$> mapM getReplacement words
  where
    getReplacement word = DB.queryNamed conn "SELECT id, word, replacement \
        \FROM replacements WHERE word = :word COLLATE NOCASE"
            [":word" := word] :: IO [Replacement]

{- | Get the current probability from the state. -}
getCurrentProbability :: WordReplacerState Probability
getCurrentProbability = fst <$> get

{- | Get the database connection from the state. -}
getConnection :: WordReplacerState DB.Connection
getConnection = snd <$> get

{- | Set the probability in the state. -}
setProbability :: Probability
    -- ^ The new probability.
    -> WordReplacerState ()
setProbability newprob = modify (\(_, conn) -> (newprob, conn))

{- | Skips space on both sides of the parser. -}
token :: RequestParser a
    -- ^ Parser to skip spaces around.
    -> RequestParser a
token tok = P.spaces *> tok <* P.spaces

{- | Parses the string given and skips all whitespace around it. -}
stringToken :: String
    -- ^ String to parse.
    -> RequestParser String
stringToken = token . P.string

{- | Remove trailing whitespace and whitespace before string. -}
trim :: String
    -- ^ String to trim.
    -> String
trim = f . f
  where
    f = reverse . dropWhile Char.isSpace
