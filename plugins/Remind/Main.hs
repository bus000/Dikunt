module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Thread.Delay (delay)
import Control.Monad (foldM_, void, unless)
import Data.Aeson (decode)
import Data.List (partition)
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import qualified Text.Parsec.Number as P
import Types.BotTypes (ServerMessage(..), IRCUser(..), getNickname, getMessage)

type User = String
type Message = String

data Reminder = Reminder User Message deriving (Show, Read, Eq)

getUser :: Reminder -> User
getUser (Reminder usr _) = usr

data Request
    = Help User
    | AddTimeReminder Reminder Integer
    | AddReminder Reminder
    | Message User
  deriving (Show, Read, Eq)

data Time
    = Years Integer
    | Months Integer
    | Days Integer
    | Hours Integer
    | Minutes Integer
    | Seconds Integer
  deriving (Show, Read, Eq)

toMicro :: Time -> Integer
toMicro (Years n) = n * toMicro (Months 12)
toMicro (Months n) = n * toMicro (Days 30)
toMicro (Days n) = n * toMicro (Hours 24)
toMicro (Hours n) = n * toMicro (Minutes 60)
toMicro (Minutes n) = n * toMicro (Seconds 60)
toMicro (Seconds n) = n * 1000000

main :: IO ()
main = do
    (nick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    messages <- parseMessages nick <$> T.hGetContents stdin

    foldM_ handleMessage [] messages

handleMessage :: [Reminder] -> Request -> IO [Reminder]
handleMessage reminds (Help botnick) = do
    printHelp botnick
    return reminds
handleMessage reminds (AddTimeReminder reminder timeDelay) = do
    putStrLn "I will make sure the message is delivered"
    void (forkIO $ remind reminder timeDelay)
    return reminds
handleMessage reminds (AddReminder reminder) = do
    putStrLn "I will make sure the message is delivered"
    return $ reminder:reminds
handleMessage reminds (Message nick) = do
    printReminds userReminds
    return rest
  where
    (userReminds, rest) = partition ((nick ==) . getUser) reminds
    printReminds = mapM_ (`remind` 0)

remind :: Reminder -> Integer -> IO ()
remind (Reminder usr msg) delayAmount = do
    unless (delayAmount == 0) $ delay delayAmount
    putStrLn $ usr ++ " I was asked to remind you that \"" ++ msg ++ "\""

printHelp :: User -> IO ()
printHelp nick = do
    putStrLn $ nick ++ ": remind <user> in 200 seconds \"<message>\" " ++
        "- In 200 seconds print <message> to <user>"
    putStrLn $ nick ++ ": remind <user> \"<message>\" - Next time <user> " ++
        "writes a message print <message>"
    putStrLn $ nick ++ ": remind <user> in <amount> <unit> \"<message>\" - " ++
        "In <amount> <unit> print <message> to <user> where unit is in " ++
        "[year, month, day, hour, minute, second] user is either a nick or " ++
        "the string \"me\" and amount is either \"a \"an\" or a number"
    putStrLn $ nick ++ ": remind help - Print this message.\", \"an\" or a " ++
        "number"

parseMessages :: User -> T.Text -> [Request]
parseMessages botnick =
    concatMap getRequests . mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    getRequests (ServerPrivMsg (IRCUser nick _ _) _ str) =
        case P.parse (request botnick (getNickname nick)) "" (getMessage str) of
            Left _ -> [Message $ getNickname nick]
            Right req -> [Message (getNickname nick), req]
    getRequests _ = []

type RequestParser a = P.Parsec String () a

request :: User -> User -> RequestParser Request
request botnick nick = stringToken (botnick ++ ":") *> stringToken "remind" *>
    (P.try (helpRequest botnick) <|> P.try (reminderRequest nick)) <* P.eof

helpRequest :: User -> RequestParser Request
helpRequest botnick = stringToken "help" *> return (Help botnick)

reminderRequest :: User -> RequestParser Request
reminderRequest from = consRemind <$> token (user from) <*>
    token (P.optionMaybe time) <*> token message
  where
    consRemind toUser Nothing msg = AddReminder (Reminder toUser msg)
    consRemind toUser (Just delayTime) msg =
        AddTimeReminder (Reminder toUser msg) (toMicro delayTime)

user :: User -> RequestParser User
user myname = P.try (me myname) <|> P.try otherUser

me :: User -> RequestParser User
me myname = stringToken "me " *> return myname

otherUser :: RequestParser User
otherUser = P.manyTill P.anyChar (P.try P.space)

time :: RequestParser Time
time = do
    void $ stringToken "in "
    am <- amount
    unit am

amount :: RequestParser Integer
amount = P.nat <|> letterAmount

letterAmount :: RequestParser Integer
letterAmount = (P.try (stringToken "an") <|> P.try (stringToken "a")) *> return 1

unit :: Integer -> RequestParser Time
unit am = P.choice $ map P.try
    [ stringToken "years" >> return (Years am)
    , stringToken "year" >> return (Years am)
    , stringToken "months" >> return (Months am)
    , stringToken "month" >> return (Months am)
    , stringToken "days" >> return (Days am)
    , stringToken "day" >> return (Days am)
    , stringToken "hours" >> return (Hours am)
    , stringToken "hour" >> return (Hours am)
    , stringToken "minutes" >> return (Minutes am)
    , stringToken "minute" >> return (Minutes am)
    , stringToken "seconds" >> return (Seconds am)
    , stringToken "second" >> return (Seconds am)
    ]

message :: RequestParser Message
message = P.between quote quote (P.many (P.noneOf "\n\r\""))
  where
    quote = P.char '"'

token :: RequestParser a -> RequestParser a
token tok = P.spaces *> tok <* P.spaces

stringToken :: String -> RequestParser String
stringToken = token . P.string
