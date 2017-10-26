{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (foldM_, void)
import Data.Aeson (decode)
import qualified Data.Configurator as Conf
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Paths_Dikunt
import Prelude hiding (lines)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import System.Random (randomRs, newStdGen)
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Types.BotTypes as BT

type BotNick = String
type Probability = Double

data Request
    = HelpRequest
    | ChangeProbabilityRequest Probability
    | OtherMessage

main :: IO ()
main = do
    (botnick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    -- Load configuration.
    configName <- getDataFileName "data/dikunt.config"
    config <- Conf.load [ Conf.Required configName ]
    initialProb <- Conf.require config "asked-probability"

    randoms <- randomRs (0.0, 1.0) <$> newStdGen
    messages <- parseRequests botnick <$> T.hGetContents stdin

    foldM_ (handleRequest botnick) initialProb $ zip messages randoms

handleRequest :: BotNick -> Probability -> (Request, Probability) -> IO Probability
handleRequest botnick threshold (HelpRequest, _) =
    giveHelp botnick *> return threshold
handleRequest _ _ (ChangeProbabilityRequest newprob, _) =
    putStrLn "Updated probability" *> return newprob
handleRequest _ threshold (OtherMessage, p)
    | p <= threshold = putStrLn "Spurgt!" *> return threshold
    | otherwise = return threshold

giveHelp :: BotNick -> IO ()
giveHelp botnick = do
    putStrLn $ botnick ++ ": asked help - Display this message"
    putStrLn $ botnick ++ ": asked set probability <0-1> - Set probability to "
        ++ "a number between 0 and 1"
    putStrLn $ "Otherwise prints \"Spurgt\" with the current probability to "
        ++ "each message"

type RequestParser a = P.Parsec String () a

parseRequests :: BotNick -> T.Text -> [Request]
parseRequests botnick =
    mapMaybe parseRequest . mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    parseRequest (BT.ServerPrivMsg _ _ msg) = plugDefault OtherMessage $
        P.parse (request botnick) "" (BT.getMessage msg)
    parseRequest _ = Nothing

    plugDefault d (Left _) = Just d
    plugDefault _ (Right b) = Just b

request :: BotNick -> RequestParser Request
request botnick = do
    void $ stringToken (botnick ++ ": ")
    void $ stringToken "asked "
    P.choice [helpRequest, changeRequest] <* P.eof

helpRequest :: RequestParser Request
helpRequest = stringToken "help" *> return HelpRequest

changeRequest :: RequestParser Request
changeRequest = do
    void $ stringToken "set "
    void $ stringToken "probability "
    newprob <- P.floating

    if newprob >= 0.0 && newprob <= 1.0
        then return $ ChangeProbabilityRequest newprob
        else P.unexpected "Probability should be between 0 and 1"

token :: RequestParser a -> RequestParser a
token tok = P.spaces *> tok <* P.spaces

stringToken :: String -> RequestParser String
stringToken = token . P.string
