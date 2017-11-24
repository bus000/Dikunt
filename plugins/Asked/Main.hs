{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Error.Util (hush)
import Control.Monad (unless, void, (>=>), when, forever)
import qualified Control.Monad.RWS as RWS
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.Configurator as Conf
import Data.Maybe (fromJust, fromMaybe, isNothing)
import qualified Data.Random as R
import qualified Data.Random.Distribution.Bernoulli as R
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Paths_Dikunt
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Prelude.Text as PT
import Prelude hiding (lines)
import qualified System.Environment as S
import qualified System.IO as S
import qualified Text.Parsec as Parse
import qualified Text.Parsec.Number as Parse
import qualified Types.BotTypes as BT

type BotNick = String
type Probability = Double

data Request
    = HelpRequest
    | ChangeProbabilityRequest Probability
    | OtherMessage
  deriving (Show, Eq)

main :: IO ()
main = do
    (botnick:_) <- S.getArgs

    S.hSetBuffering S.stdout S.LineBuffering
    S.hSetBuffering S.stdin S.LineBuffering

    -- Load configuration.
    configName <- getDataFileName "data/dikunt.config"
    config <- Conf.load [ Conf.Required configName ]
    initialProb <- Conf.require config "asked-probability"

    let pipe = PT.stdinLn >-> P.map T.encodeUtf8 >-> parseRequest >->
            handleRequest >-> PT.stdoutLn
    runAsked botnick initialProb $ P.runEffect pipe

type Asked = RWS.RWST BotNick () (R.RVar Bool) (R.RVarT IO)

runAsked :: BotNick -> Probability -> Asked a -> IO a
runAsked botnick initialProb a =
    R.runRVarT (fst <$> RWS.evalRWST a botnick (R.bernoulli initialProb))
        R.StdRandom

parseRequest :: P.Pipe B.ByteString Request Asked ()
parseRequest = forever $ do
    botnick <- RWS.ask
    req <- (JSON.decodeStrict >=> parse botnick) <$> P.await
    unless (isNothing req) $ P.yield (fromJust req)
  where
    parse botnick (BT.ServerPrivMsg _ _ msg)
        = Just
        . fromMaybe OtherMessage
        . hush
        . Parse.parse (request botnick) ""
        . BT.getMessage
        $ msg
    parse _ _ = Nothing

handleRequest :: P.Pipe Request T.Text Asked ()
handleRequest = forever $ do
    req <- P.await
    case req of
        HelpRequest -> giveHelp
        ChangeProbabilityRequest newProb -> updateProb newProb
        OtherMessage -> handleOther

giveHelp :: P.Pipe Request T.Text Asked ()
giveHelp = do
    botnick <- RWS.ask
    P.yield $ T.pack botnick <> ": asked help - Display this message"
    P.yield $ T.pack botnick <> ": asked set probability <0-1> - Set " <>
        "probability to a number between 0 and 1"
    P.yield $ T.pack "Otherwise prints \"Spurgt\" with the current " <>
        "probability to each message"

updateProb :: Probability -> P.Pipe Request T.Text Asked ()
updateProb = RWS.put . R.bernoulli >=> const (P.yield "Updated probability")

handleOther :: P.Pipe Request T.Text Asked ()
handleOther = do
    b <- RWS.get
    shouldAsk <- P.lift . RWS.lift $ R.sample b
    when shouldAsk $ P.yield "Spurgt!"

type RequestParser a = Parse.Parsec String () a

request :: BotNick -> RequestParser Request
request botnick = do
    void $ stringToken (botnick ++ ": ")
    void $ stringToken "asked "
    Parse.choice [helpRequest, changeRequest] <* Parse.eof

helpRequest :: RequestParser Request
helpRequest = stringToken "help" *> return HelpRequest

changeRequest :: RequestParser Request
changeRequest = do
    void $ stringToken "set "
    void $ stringToken "probability "
    newprob <- Parse.floating

    if newprob >= 0.0 && newprob <= 1.0
        then return $ ChangeProbabilityRequest newprob
        else Parse.unexpected "Probability should be between 0 and 1"

token :: RequestParser a -> RequestParser a
token tok = Parse.spaces *> tok <* Parse.spaces

stringToken :: String -> RequestParser String
stringToken = token . Parse.string
