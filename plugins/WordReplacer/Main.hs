{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import qualified BotTypes as BT
import Text.Regex.PCRE ((=~))
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Paths_Dikunt
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple (NamedParam((:=)))
import Control.Monad (foldM, forever)
import Safe (readMay)
import System.Environment (getArgs)
import qualified Data.Text as T
import Control.Applicative ((<$>), (<*>))

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
    DB.withConnection dbFile (wordReplacer nick)

wordReplacer :: String -> DB.Connection -> IO ()
wordReplacer nick conn = do
    DB.execute_ conn "CREATE TABLE IF NOT EXISTS replacements \
        \(id INTEGER PRIMARY KEY, word TEXT UNIQUE, replacement TEXT)"

    forever $ do
        line <- getLine
        case readMay line :: Maybe BT.Message of
            Just (BT.PrivMsg _ _ str) -> handleInput conn nick str
            _ -> return ()

handleInput :: DB.Connection -> String -> String -> IO ()
handleInput conn nick str
    | str =~ helpPattern nick = help nick
    | str =~ addPattern nick = case str =~ addPattern nick of
        [[_, word, replacement]] -> addReplacement conn word replacement
        _ -> return ()
    | otherwise = replaceWords conn str

sp, ps :: String
sp = "[ \\t]*"
ps = "[ \\t]+"

helpPattern, addPattern :: String -> String
helpPattern nick = concat ["^", sp, nick, "\\:", ps, "wordreplacer", ps, "help",
    sp, "$"]
addPattern nick = concat ["^", sp, nick, "\\:", ps, "wordreplacer", ps, "add",
    ps, "([a-zA-Z0-9]*)=([a-zA-Z0-9]*)"]

help :: String -> IO ()
help nick = putStrLn $ unlines
    [ unwords [nick, "wordreplacer add <word1>=<word2> - add word1=word2 to"
        , "database"]
    , unwords [nick, "wordreplacer help - display this message"]
    , "otherwise replaces words from database in messages"
    ]

addReplacement :: DB.Connection -> String -> String -> IO ()
addReplacement conn word replacement = do
    DB.executeNamed conn "INSERT OR REPLACE INTO replacements \
        \(word, replacement) VALUES (:word, :replacement)" [":word" := word,
        ":replacement" := replacement]

    putStrLn "Added replacement"

replaceWords :: DB.Connection -> String -> IO ()
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

replaceStrings :: [Replacement] -> String -> String
replaceStrings replacements str = unwords . map maybeReplace . words $ str
  where
    replacementList = map getReplacement replacements
    maybeReplace word = case lookup word replacementList of
        Just newWord -> newWord
        Nothing -> word

getReplacement :: Replacement -> (String, String)
getReplacement (Replacement _ word replacement) =
    (T.unpack word, T.unpack replacement)

{- TODO: Make default replacements. -}
{-replacementList :: [(String, String)]-}
{-replacementList =-}
    {-[ ("Mark", "ShortGuy")-}
    {-, ("Jan", "Tjekkeren")-}
    {-, ("Magnus", "Glorious")-}
    {-, ("August", "Motherless")-}
    {-, ("Oleks", "Joleks")-}
    {-, ("10/10", "knæhøj karse")-}
    {-, ("ha!", "HAHAHAHAHAHHAHA!")-}
    {-, ("haha", "hilarious, just hilarious")-}
    {-, ("Mads", "Twin")-}
    {-, ("Troels", "Twin")-}
    {-, ("tjekker", "kontrollerer")-}
    {-, ("Tjekker", "Kontrollerer")-}
    {-, ("tjekke", "kontrollere")-}
    {-, ("vim", "best editor")-}
    {-, ("emacs", "worst editor")-}
    {-, ("fucking", "")-}
    {-, ("fuck", "")-}
    {-, ("Fucking", "")-}
    {-, ("Fuck", "")-}
    {-, ("Helvede", "Himlen")-}
    {-, ("helvede", "himlen")-}
    {-, ("0/10", "Mark's Bot")-}
    {-, ("danmark", "I DANMARK ER JEG FØDT DER HAR JEG HJEEEEEME")-}
    {-, ("Danmark", "I DANMARK ER JEG FØDT DER HAR JEG HJEEEEEME")-}
    {-, ("USA", "Murica'")-}
    {-, ("Margrethe", "Hendes Majestæt Dronning Margrethe II")-}
    {-, ("Henrik", "Hans Kongelige Højhed Prins Henrik")-}
    {-, ("Frederik", "Hans Kongelige Højhed Kronprins Frederik, Prins til Danmark, greve af Monpezat")-}
    {-, ("Joakim", "Hans Kongelige Højhed Prins Joachim")-}
    {-, ("Mary", "Hendes Kongelige Højhed Kronprinsesse Mary")-}
    {-, ("python", "python2.7")-}
    {-, ("Python", "Python2.7")-}
    {-, ("latex", "LaTeX")-}
    {-, ("tex", "Tex")-}
    {-]-}
