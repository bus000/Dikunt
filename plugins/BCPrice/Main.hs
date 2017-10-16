{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Error.Util (hush)
import Control.Exception (Exception, throw)
import qualified Control.Monad.Except as E
import qualified Control.Monad.State as S
import Data.Aeson (decode, eitherDecodeStrict, FromJSON(..), withObject, (.:))
import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Time as Time
import Network.Download (openURI)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import Text.Printf (printf)
import qualified Types.BotTypes as BT

type BotNick = String

{- | Incoming request to the bot. -}
data Request
    = Help BotNick
    | GetPrice
    | BuyCoins
  deriving (Show, Read, Eq)

{- | Bitcoin price. The 4 least significant digits in base 10 is cents. -}
newtype BCPrice = BCPrice Int deriving (Show, Read, Eq, Ord)

instance FromJSON BCPrice where
    parseJSON = withObject "whole" $ \whole -> do
        bpi <- whole .: "bpi"
        usd <- bpi .: "USD"
        priceText <- usd .: "rate"

        case P.parse P.int "" (filter (`elem` ['0'..'9']) priceText) of
            Right price -> return $ BCPrice price
            Left err -> fail $ show err

{- | Stores the price of a bitcoin at a specific time. -}
data BCState = BCState BCPrice Time.UTCTime

{- | Thrown if initialization of the plugin fails. -}
data InitializationError = InitializationError deriving Show

instance Exception InitializationError

{- | Supplier of the bitcoin price. -}
priceSource :: String
priceSource = "http://api.coindesk.com/v1/bpi/currentprice.json"

main :: IO ()
main = do
    (botnick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    messages <- parseMessages botnick <$> T.hGetContents stdin

    initialPrice <- either (const $ throw InitializationError) id <$> getPrice
    currentTime <- Time.getCurrentTime
    let initialState = BCState initialPrice currentTime

    S.evalStateT (mapM_ handleRequest messages) initialState

handleRequest :: Request -> S.StateT BCState IO ()
handleRequest (Help botnick) = S.liftIO $ giveHelp botnick
handleRequest GetPrice = printPrice
handleRequest BuyCoins = S.liftIO buyCoins

{- | Print plugin help. -}
giveHelp :: BotNick -> IO ()
giveHelp botnick = do
    putStrLn $ botnick ++ ": bcprice help - Display this message"
    putStrLn $ botnick ++ ": bcprice buy <amount> <card number> <CSC> - Buy " ++
        "<amount> bitcoins using the credit card information given"
    putStrLn $ botnick ++ ": bcprice - Print current bitcoin price"

{- | Get and print the current price of the bitcoin. Update the state with the
 - new price and new timestamp. -}
printPrice :: S.StateT BCState IO ()
printPrice = do
    (BCState oldPrice time) <- S.get
    newPrice <- S.liftIO getPrice
    currentTime <- S.liftIO Time.getCurrentTime
    let timeDiff = Time.diffUTCTime currentTime time

    case newPrice of
        Right t -> do
            S.put $ BCState t currentTime
            S.liftIO . putStrLn . format timeDiff oldPrice $ t
        Left err -> S.liftIO $ putStrLn err
  where
    format time oldPrice newPrice | oldPrice < newPrice = unwords
        ["Prisen er steget fra"
        , formatPrice oldPrice
        , "til"
        , formatPrice newPrice
        , "i de sidste"
        , show $ dayNumber time
        , "dage"
        ]
    format time oldPrice newPrice | oldPrice > newPrice = unwords
        ["Prisen er faldet fra"
        , formatPrice oldPrice
        , "til"
        , formatPrice newPrice
        , "i de sidste"
        , show $ dayNumber time
        , "dage"
        ]
    format time oldPrice _ = unwords
        ["Prisen er forblevet på"
        , formatPrice oldPrice
        , "i de sidste"
        , show $ dayNumber time
        , "dage"
        ]

{- | Format the price of a bitcoin in US Dollar. -}
formatPrice :: BCPrice -> String
formatPrice (BCPrice price) = "$" ++ withCommas wholePart ++ "." ++ decimalPart
  where
    wholePart = show (price `div` 10000)
    decimalPart = printf "%04u" (price `mod` 10000)
    withCommas = reverse . L.intercalate "," . L.chunksOf 3 . reverse

{- | Get the floor of the number of days in a time period. -}
dayNumber :: Time.NominalDiffTime -> Integer
dayNumber time = floor (realToFrac (time / Time.nominalDay) :: Double)

{- | Get the current bitcoin price or return an error. -}
getPrice :: IO (Either String BCPrice)
getPrice = E.runExceptT $ do
    json <- E.ExceptT (openURI priceSource)
    E.ExceptT $ return (eitherDecodeStrict json)

buyCoins :: IO ()
buyCoins = putStrLn "Tak for dit køb. Dine BitCoins vil ankomme den 9/11."

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
    token (P.many P.digit) *> token cardNumber *> token csc *>
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
