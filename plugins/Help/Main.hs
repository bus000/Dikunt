{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Error.Util (hush)
import Data.Aeson (decode)
import Data.Char (toLower)
import Data.List as L
import Data.List.Split as L
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import qualified Text.Parsec as P
import qualified Types.BotTypes as BT

type BotNick = String
type Plugins = [FilePath]

data Request = HelpRequest | PluginHelpRequest

main :: IO ()
main = do
    (botnick:_:plugins:_) <- getArgs

    let executables = L.splitOn ";" . map toLower $ plugins

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    requests <- parseRequests botnick <$> T.hGetContents stdin

    mapM_ (handleRequest botnick executables) requests

handleRequest :: BotNick -> Plugins -> Request -> IO ()
handleRequest botnick _ HelpRequest = giveHelp botnick
handleRequest botnick plugins PluginHelpRequest = givePluginHelp botnick plugins

giveHelp :: BotNick -> IO ()
giveHelp botnick = do
    putStrLn $ botnick ++ ": help help - Display this message"
    putStrLn $ botnick ++ ": help - Display how to get help for other modules"

givePluginHelp :: BotNick -> Plugins -> IO ()
givePluginHelp botnick executables = do
    putStrLn $ "Try the command \"" ++ botnick ++ ": <plugin> help\"."
    putStrLn $ "Where <plugin> is one of " ++ L.intercalate ", " executables

parseRequests :: BotNick -> T.Text -> [Request]
parseRequests botnick =
    mapMaybe getRequest . mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    getRequest (BT.ServerPrivMsg _ _ msg) =
        hush $ P.parse (request botnick) "" (BT.getMessage msg)
    getRequest _ = Nothing

type RequestParser a = P.Parsec String () a

request :: BotNick -> RequestParser Request
request botnick = stringToken (botnick ++ ": ") *> stringToken "help" *>
    P.choice requests <* P.eof
  where
    requests =
        [ stringToken "help" *> return HelpRequest
        , return PluginHelpRequest
        ]

token :: RequestParser a -> RequestParser a
token tok = P.spaces *> tok <* P.spaces

stringToken :: String -> RequestParser String
stringToken = token . P.string
