{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Data.Maybe (fromMaybe)
import qualified Types.BotTypes as BT
import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM, forever, unless)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple (NamedParam((:=)))
import Paths_Dikunt
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.Regex.PCRE ((=~))
import Data.List.Split (split, oneOf)
import Data.Char (toLower)
import Data.Aeson (decode)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

data Replacement = Replacement Int T.Text T.Text deriving (Show)

instance DB.FromRow Replacement where
    fromRow = Replacement <$> DB.field <*> DB.field <*> DB.field

instance DB.ToRow Replacement where
    toRow (Replacement id_ word replacement) = DB.toRow (id_, word, replacement)

main :: IO ()
main = do
    (nick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    dbFile <- getDataFileName "data/WordReplacerData.db"
    DB.withConnection dbFile (\c -> initDatabase c >> wordReplacer nick c)

{- | Initialize a database by creating it if it does not exist and inserting
 - default mappings. -}
initDatabase :: DB.Connection
    -- ^ Connection to database to initialize.
    -> IO ()
initDatabase conn = do
    DB.execute_ conn "CREATE TABLE IF NOT EXISTS replacements \
        \(id INTEGER PRIMARY KEY, word TEXT UNIQUE, replacement TEXT)"
    mapM_ insertDefault replacementList
  where
    insertDefault (word, replacement) =
        DB.executeNamed conn "INSERT OR REPLACE INTO replacements \
            \(word, replacement) VALUES (:word, :replacement)"
                [":word" := word
                , ":replacement" := replacement
                ]

{- | Do the word replacements. Reads lines from stdin replaces words and prints
 - the replacement if any to stdout. -}
wordReplacer :: String
    -- ^ Nickname of Dikunt bot.
    -> DB.Connection
    -- ^ Database connection with replacements.
    -> IO ()
wordReplacer nick conn = forever $ do
    line <- T.getLine
    handleMessage conn nick $ (decode . T.encodeUtf8) line

{- | Parse message and determine if it is a help message, an add message or if
 - the words should be replaced. -}
handleMessage :: DB.Connection
    -- ^ Database connection.
    -> String
    -- ^ Nickname of Dikunt bot.
    -> Maybe BT.ServerMessage
    -- ^ Message received from IRCServer.
    -> IO ()
handleMessage conn nick (Just (BT.ServerPrivMsg _ _ str))
    | BT.getMessage str =~ helpPattern = help nick
    | [[_, word, replacement]] <- BT.getMessage str =~ addPattern =
        addReplacement conn nick word replacement
    | otherwise = replaceWords conn (BT.getMessage str)
  where
    sp = "[ \\t]*"
    ps = "[ \\t]+"
    helpPattern = concat ["^", sp, nick, ":", ps, "wordreplacer", ps, "help",
        sp, "$"]
    addPattern = concat ["^", sp, nick, ":", ps, "wordreplacer", ps, "add", ps,
        "([^= ]+)=([^=]*)", sp, "$"]
handleMessage _ _ _ = return ()

{- | Print help message. -}
help :: String
    -- ^ Nickname of Dikunt bot.
    -> IO ()
help nick = putStrLn $ unlines
    [ concat [nick, ": wordreplacer add <word1>=<word2> - add word1=word2 to "
        , "database"]
    , concat [nick, ": wordreplacer help - display this message "]
    , "otherwise replaces words from database in messages"
    ]

{- | Add a new replacement to the database. -}
addReplacement :: DB.Connection
    -- ^ Connection to database.
    -> String
    -- ^ Nick of the bot.
    -> String
    -- ^ The word to replace.
    -> String
    -- ^ The replacement for the word.
    -> IO ()
addReplacement conn nick word replacement
    | nick == word' = putStrLn "Not allowed to bind nickname"
    | otherwise = do
        DB.executeNamed conn "INSERT OR REPLACE INTO replacements \
            \(word, replacement) VALUES (:word, :replacement)"
            [":word" := word', ":replacement" := replacement]

        putStrLn "Added replacement"
  where
    word' = map toLower word

{- | Replace all words present in the database with their replacements. -}
replaceWords :: DB.Connection
    -- ^ Database connection.
    -> String
    -- ^ String to replace in.
    -> IO ()
replaceWords conn line = do
    replacements <- foldM getReplacements [] (wordsSpecial . map toLower $ line)
    unless (null replacements) $ putStrLn (replaceStrings replacements line)
  where
    getReplacements replacements word = do
        r <- DB.queryNamed conn "SELECT id, word, replacement FROM \
            \replacements WHERE word = :word"
            [":word" := word] :: IO [Replacement]
        if null r then return replacements else return $ head r:replacements

{- | Replace words in string with their replacement in the replacementlist
 - given. -}
replaceStrings :: [Replacement]
    -- ^ Word replacements.
    -> String
    -- ^ The string to replace in.
    -> String
replaceStrings replacements = concatMap maybeReplace . wordsSpecial
  where
    replacements' = map getReplacement replacements
    maybeReplace word = fromMaybe word (lookup (map toLower word) replacements')

{- | Utility function converting Replacement's to tuples of replacement. -}
getReplacement :: Replacement
    -- ^ The replacements.
    -> (String, String)
getReplacement (Replacement _ word replacement) =
    (T.unpack word, T.unpack replacement)

{- | List of default replacements. -}
replacementList :: [(String, String)]
replacementList =
    [ ("mark", "ShortGuy")
    , ("jan", "Tjekkeren")
    , ("magnus", "Glorious")
    , ("august", "Motherless")
    , ("oleks", "Joleks")
    , ("10/10", "knæhøj karse")
    , ("ha!", "HAHAHAHAHAHHAHA!")
    , ("haha", "hilarious, just hilarious")
    , ("mads", "Twin")
    , ("troels", "Twin")
    , ("tjekker", "kontrollerer")
    , ("tjekke", "kontrollere")
    , ("vim", "best editor")
    , ("emacs", "worst editor")
    , ("helvede", "himlen")
    , ("0/10", "Mark's Bot")
    , ("danmark", "I DANMARK ER JEG FØDT DER HAR JEG HJEEEEEME")
    , ("usa", "Murica'")
    , ("margrethe", "Hendes Majestæt Dronning Margrethe II")
    , ("henrik", "Hans Kongelige Højhed Prins Henrik")
    , ("frederik", "Hans Kongelige Højhed Kronprins Frederik, Prins til Danmark, greve af Monpezat")
    , ("joakim", "Hans Kongelige Højhed Prins Joachim")
    , ("mary", "Hendes Kongelige Højhed Kronprinsesse Mary")
    , ("python", "python2.7")
    , ("latex", "LaTeX")
    , ("tex", "Tex")
    ]

wordsSpecial :: String -> [String]
wordsSpecial = split (oneOf " \n\t.:?!")
