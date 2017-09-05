{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Error.Util (hush)
import Control.Monad (mapM_)
import Data.Aeson (decode, FromJSON(..), withObject, (.:))
import Data.ByteString.Builder (toLazyByteString, byteString)
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Network.Download (openURI)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import qualified Text.Parsec as P
import qualified Types.BotTypes as BT

type BotNick = String

data Request
    = Help BotNick
    | GetPrice
    | BuyCoins
  deriving (Show, Read, Eq)

priceSource :: String
priceSource = "http://api.coindesk.com/v1/bpi/currentprice.json"

main :: IO ()
main = do
    (botnick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    messages <- parseMessages botnick <$> T.hGetContents stdin

    mapM_ handleRequest messages

handleRequest :: Request -> IO ()
handleRequest (Help botnick) = giveHelp botnick
handleRequest GetPrice = getPrice
handleRequest BuyCoins = buyCoins

giveHelp :: BotNick -> IO ()
giveHelp botnick = do
    putStrLn $ botnick ++ ": bcprice help - Display this message"
    putStrLn $ botnick ++ ": bcprice buy <amount> <card number> <CSC> - Buy " ++
        "<amount> bitcoins using the credit card information given"
    putStrLn $ botnick ++ ": bcprice - Print current bitcoin price"

getPrice :: IO ()
getPrice = do
    priceJSON <- openURI priceSource
    case priceJSON of
        Left _ -> putStrLn "Kunne ikke hente prisen"
        Right json -> case (decode . toLazyByteString . byteString) json of
            Nothing -> putStrLn "Kunne ikke parse JSON"
            Just (BCPrice price) -> putStrLn price

buyCoins :: IO ()
buyCoins = putStrLn "Tak for dit køb. Dine BitCoins vil ankomme den 9/11."

newtype BCPrice = BCPrice String deriving (Show, Read, Eq)

instance FromJSON BCPrice where
    parseJSON = withObject "whole" $ \whole -> do
        bpi <- whole .: "bpi"
        usd <- bpi .: "USD"
        price <- usd .: "rate"

        return (BCPrice price)

parseMessages :: BotNick -> T.Text -> [Request]
parseMessages botnick =
    mapMaybe getRequest . mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    getRequest (BT.ServerPrivMsg _ _ msg) =
        hush $ P.parse (request botnick) "" (BT.getMessage msg)
    getRequest _ = Nothing

type RequestParser a = P.Parsec String () a

request :: BotNick -> RequestParser Request
request botnick = stringToken (botnick ++ ":") *> P.choice requestTypes <* P.eof
  where
    requestTypes = map P.try
        [ helpRequest botnick
        , buyCoinsRequest
        , getPriceRequest
        ]

helpRequest :: BotNick -> RequestParser Request
helpRequest botnick = stringToken "bcprice" *> stringToken "help" *>
    return (Help botnick)

buyCoinsRequest :: RequestParser Request
buyCoinsRequest = stringToken "bcprice" *> stringToken "buy" *>
    (token $ P.many P.digit) *> (token cardNumber) *> (token csc) *>
    return BuyCoins
  where
    cardNumber = P.count 4 $ P.count 4 P.digit <* P.optional (P.char ' ')
    csc = P.count 3 P.digit

getPriceRequest :: RequestParser Request
getPriceRequest = stringToken "bcprice" *> return GetPrice

token :: RequestParser a -> RequestParser a
token tok = P.spaces *> tok <* P.spaces

stringToken :: String -> RequestParser String
stringToken = token . P.string
