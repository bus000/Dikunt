module Main (main) where

import Control.Error.Util (hush)
import Data.Aeson (decode)
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import System.Process (readProcessWithExitCode)
import qualified Text.Parsec as P
import qualified Types.BotTypes as BT

type BotNick = String
type Expression = String

data Request
    = HelpRequest BotNick
    | EvalRequest String
  deriving (Show, Read, Eq)

main :: IO ()
main = do
    (botnick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    requests <- parseRequests botnick <$> T.hGetContents stdin

    mapM_ handleRequest requests

handleRequest :: Request -> IO ()
handleRequest (HelpRequest botnick) = giveHelp botnick
handleRequest (EvalRequest req) = evalRequest req

giveHelp :: BotNick -> IO ()
giveHelp botnick = do
    putStrLn $ botnick ++ ": eval help - Display this message"
    putStrLn $ botnick ++ ": eval <request> - Evaluates the <request> as a "
        ++ "Haskell expression"

evalRequest :: Expression -> IO ()
evalRequest req = do
    (_, out, _) <- readProcessWithExitCode "mueval" args ""
    putStrLn out
  where
    args = ["--expression", req]

parseRequests :: BotNick -> T.Text -> [Request]
parseRequests botnick =
    mapMaybe getRequest . mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    getRequest (BT.ServerPrivMsg _ _ msg) =
        hush $ P.parse (request botnick) "" (BT.getMessage msg)
    getRequest _ = Nothing

type RequestParser a = P.Parsec String () a

request :: BotNick -> RequestParser Request
request botnick = stringToken (botnick ++ ": ") *> stringToken "eval" *>
    P.choice requests <* P.eof
  where
    requests = map P.try
        [ helpRequest botnick
        , parseEvalRequest
        ]

helpRequest :: BotNick -> RequestParser Request
helpRequest botnick = stringToken "help" *> return (HelpRequest botnick)

parseEvalRequest :: RequestParser Request
parseEvalRequest = EvalRequest <$> P.many1 (P.noneOf "\r\n")

token :: RequestParser a -> RequestParser a
token tok = P.spaces *> tok <* P.spaces

stringToken :: String -> RequestParser String
stringToken = token . P.string
