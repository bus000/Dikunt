{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Error.Util (hush)
import Control.Monad (forever, unless, (>=>))
import qualified Control.Monad.Reader as R
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import Data.Maybe (isNothing, fromJust)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Prelude.Text as PT
import qualified System.Environment as S
import qualified System.IO as S
import qualified Text.Parsec as Parse
import qualified Types.BotTypes as BT

data Request
    = HelpRequest
    | Message T.Text
  deriving (Show)

type BotNick = String

main :: IO ()
main = do
    (botnick:_) <- S.getArgs

    S.hSetBuffering S.stdout S.LineBuffering
    S.hSetBuffering S.stdin S.LineBuffering

    R.runReaderT (P.runEffect pipe) botnick
  where
    pipe = PT.stdinLn
        >-> P.map T.encodeUtf8
        >-> parseRequest
        >-> handleRequest
        >-> PT.stdoutLn

parseRequest :: P.Pipe B.ByteString Request (R.ReaderT BotNick IO) ()
parseRequest = forever $ do
    botnick <- R.ask
    req <- (JSON.decodeStrict >=> parse botnick) <$> P.await
    unless (isNothing req) $ P.yield (fromJust req)
  where
    parse botnick (BT.ServerPrivMsg _ _ msg)
        = hush
        . Parse.parse (request botnick) ""
        . BT.getMessage
        $ msg
    parse _ _ = Nothing

handleRequest :: P.Pipe Request T.Text (R.ReaderT BotNick IO) ()
handleRequest = forever $ do
    req <- P.await
    case req of
        HelpRequest -> giveHelp
        Message msg -> handleMessage msg

giveHelp :: P.Pipe a T.Text (R.ReaderT BotNick IO) ()
giveHelp = do
    botnick <- R.ask
    P.yield $ T.pack botnick <> ": merry help - Display this string"
    P.yield "Otherwise prints christmas greetings."

handleMessage :: T.Text -> P.Pipe a T.Text (R.ReaderT BotNick IO) ()
handleMessage msg
    | "glædelig jul" `T.isInfixOf` lower = P.yield merryChristmas
    | "glaedelig jul" `T.isInfixOf` lower = P.yield merryChristmas
    | "godt nytår" `T.isInfixOf` lower = P.yield merryChristmas
    | "godt nytaar" `T.isInfixOf` lower = P.yield merryChristmas
    | "godmorgen" `T.isInfixOf` lower = P.yield goodMorning
    | "mojn" `T.isInfixOf` lower = P.yield mojn
    | "godaften" `T.isInfixOf` lower = P.yield godaften
    | otherwise = return ()
  where
    lower = T.toLower msg
    merryChristmas = "Glædelig jul og godt nytår til dig!"
    goodMorning = "Godmorgen!"
    mojn = "Mojn, mojn!"
    godaften = "Godaften!"

type RequestParser a = Parse.Parsec String () a

request :: BotNick -> RequestParser Request
request botnick = Parse.choice [helpRequest, messageRequest] <* Parse.eof
  where
    helpRequest =
           stringToken (botnick <> ": ")
        *> stringToken "merry "
        *> stringToken "help"
        *> pure HelpRequest
    messageRequest = Message . T.pack <$> Parse.many Parse.anyChar

stringToken :: String -> RequestParser String
stringToken = token . Parse.string

token :: RequestParser a -> RequestParser a
token tok = Parse.spaces *> tok <* Parse.spaces
