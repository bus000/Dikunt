{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import qualified BotTypes as BT
import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM, forever)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple (NamedParam((:=)))
import Paths_Dikunt
import Safe (readMay)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.Regex.PCRE ((=~))

data Replacement = Replacement Int T.Text T.Text deriving (Show)

instance DB.FromRow Replacement where
    fromRow = Replacement <$> DB.field <*> DB.field <*> DB.field

instance DB.ToRow Replacement where
    toRow (Replacement id_ word replacement) = DB.toRow (id_, word, replacement)

main :: IO ()
main = do
    [nick, _] <- getArgs

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
    mapM_ (insertDefault conn) replacementList
  where
    insertDefault c (word, replacement) =
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
wordReplacer nick conn = do
    forever $ do
        line <- getLine
        handleMessage conn nick $ readMay line

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
    | str =~ helpPattern nick = help nick
    | str =~ addPattern nick = case str =~ addPattern nick of
        [[_, word, replacement]] -> addReplacement conn word replacement
        _ -> return ()
    | otherwise = replaceWords conn str
  where
    sp = "[ \\t]*"
    ps = "[ \\t]+"
    helpPattern nick = concat ["^", sp, nick, "\\:", ps, "wordreplacer", ps, "help",
        sp, "$"]
    addPattern nick = concat ["^", sp, nick, "\\:", ps, "wordreplacer", ps, "add",
        ps, "([a-zA-Z0-9]*)=([a-zA-Z0-9]*)"]
handleMessage _ _ _ = return ()

{- | Print help message. -}
help :: String
    -- ^ Nickname of Dikunt bot.
    -> IO ()
help nick = putStrLn $ unlines
    [ unwords [nick, "wordreplacer add <word1>=<word2> - add word1=word2 to"
        , "database"]
    , unwords [nick, "wordreplacer help - display this message"]
    , "otherwise replaces words from database in messages"
    ]

{- | Add a new replacement to the database. -}
addReplacement :: DB.Connection
    -- ^ Connection to database.
    -> String
    -- ^ The word to replace.
    -> String
    -- ^ The replacement for the word.
    -> IO ()
addReplacement conn word replacement = do
    DB.executeNamed conn "INSERT OR REPLACE INTO replacements \
        \(word, replacement) VALUES (:word, :replacement)" [":word" := word,
        ":replacement" := replacement]

    putStrLn "Added replacement"

{- | Replace all words present in the database with their replacements. -}
replaceWords :: DB.Connection
    -- ^ Database connection.
    -> String
    -- ^ String to replace in.
    -> IO ()
replaceWords conn line = do
    replacements <- foldM getReplacements [] (words line)
    if null replacements
    then return ()
    else putStrLn $ replaceStrings replacements line
  where
    getReplacements replacements word = do
        r <- DB.queryNamed conn "SELECT id, word, replacement FROM \
            \replacements WHERE word = :word"
            [":word" := word] :: IO [Replacement]
        if null r then return replacements else return $ (head r):replacements

{- | Replace words in string with their replacement in the replacementlist
 - given. -}
replaceStrings :: [Replacement]
    -- ^ Word replacements.
    -> String
    -- ^ The string to replace in.
    -> String
replaceStrings replacements str = unwords . map maybeReplace . words $ str
  where
    replacementList = map getReplacement replacements
    maybeReplace word = case lookup word replacementList of
        Just newWord -> newWord
        Nothing -> word

{- | Utility function converting Replacement's to tuples of replacement. -}
getReplacement :: Replacement
    -- ^ The replacements.
    -> (String, String)
getReplacement (Replacement _ word replacement) =
    (T.unpack word, T.unpack replacement)

{- | List of default replacements. -}
replacementList :: [(String, String)]
replacementList =
    [ ("Mark", "ShortGuy")
    , ("Jan", "Tjekkeren")
    , ("Magnus", "Glorious")
    , ("August", "Motherless")
    , ("Oleks", "Joleks")
    , ("10/10", "knæhøj karse")
    , ("ha!", "HAHAHAHAHAHHAHA!")
    , ("haha", "hilarious, just hilarious")
    , ("Mads", "Twin")
    , ("Troels", "Twin")
    , ("tjekker", "kontrollerer")
    , ("Tjekker", "Kontrollerer")
    , ("tjekke", "kontrollere")
    , ("vim", "best editor")
    , ("emacs", "worst editor")
    , ("fucking", "")
    , ("fuck", "")
    , ("Fucking", "")
    , ("Fuck", "")
    , ("Helvede", "Himlen")
    , ("helvede", "himlen")
    , ("0/10", "Mark's Bot")
    , ("danmark", "I DANMARK ER JEG FØDT DER HAR JEG HJEEEEEME")
    , ("Danmark", "I DANMARK ER JEG FØDT DER HAR JEG HJEEEEEME")
    , ("USA", "Murica'")
    , ("Margrethe", "Hendes Majestæt Dronning Margrethe II")
    , ("Henrik", "Hans Kongelige Højhed Prins Henrik")
    , ("Frederik", "Hans Kongelige Højhed Kronprins Frederik, Prins til Danmark, greve af Monpezat")
    , ("Joakim", "Hans Kongelige Højhed Prins Joachim")
    , ("Mary", "Hendes Kongelige Højhed Kronprinsesse Mary")
    , ("python", "python2.7")
    , ("Python", "Python2.7")
    , ("latex", "LaTeX")
    , ("tex", "Tex")
    ]
