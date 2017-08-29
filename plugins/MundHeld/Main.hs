module Main (main) where

import Control.Error.Util (hush)
import Control.Monad (void)
import Data.Aeson (decode)
import Data.Char (isSpace)
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Paths_Dikunt
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Types.BotTypes as BT

type User = String
type SearchString = String
type Mundheld = String

data Request
    = Help User
    | GetMundHeld SearchString

main :: IO ()
main = do
    (nick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    -- Get all mundheld from file.
    mundheld <- lines <$> (getDataFileName "data/mundheld.txt" >>= readFile)

    requests <- parseRequests nick <$> T.hGetContents stdin

    mapM_ (handleRequest mundheld) requests

handleRequest :: [Mundheld] -> Request -> IO ()
handleRequest _ (Help botnick) = giveHelp botnick
handleRequest mundheld (GetMundHeld searchString) = giveMundHeld mundheld searchString

giveHelp :: User -> IO ()
giveHelp botnick = do
    putStrLn $ botnick ++ ": mundheld help - Display this message."
    putStrLn $ botnick ++ ": mundheld <search-string> - Find a mundheld in " ++
        "the database that matches the search string given."

giveMundHeld :: [Mundheld] -> SearchString -> IO ()
giveMundHeld mundheld search = case filter (search `isInfixOf`) mundheld of
    [] -> putStrLn "Jeg fandt ingen mundheld der matchede din søge streng."
    (x:_) -> putStrLn x

parseRequests :: User -> T.Text -> [Request]
parseRequests botnick =
    mapMaybe getRequest . mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    getRequest (BT.ServerPrivMsg _ _ msg) =
        hush $ P.parse (request botnick) "(stdin)" (BT.getMessage msg)
    getRequest _ = Nothing

request :: User -> P.Parsec String () Request
request botnick = do
    void $ token (P.string $ botnick ++ ":")
    void $ token (P.string "mundheld")
    req <- P.try (helpRequest botnick) <|> P.try mundHeldRequest
    P.eof

    return req

helpRequest :: User -> P.Parsec String () Request
helpRequest botnick = token (P.string "help") *> return (Help botnick)

mundHeldRequest :: P.Parsec String () Request
mundHeldRequest = GetMundHeld . trim <$> P.many1 (P.noneOf "\r\n")

token :: P.Parsec String () a -> P.Parsec String () a
token tok = P.spaces *> tok <* P.spaces

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
