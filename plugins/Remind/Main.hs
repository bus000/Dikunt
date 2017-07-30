module Main (main) where

import BotTypes (ServerMessage(..), IRCUser(..), getNickname)
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
import Text.ParserCombinators.Parsec
    ( GenParser
    , between
    , many
    , noneOf
    , string
    , char
    , try
    , (<|>)
    , spaces
    , eof
    , manyTill
    , anyChar
    , parse
    , optionMaybe
    , space
    )
import Text.ParserCombinators.Parsec.Number (nat)

type User = String
type Message = String

data Reminder = Reminder User Message deriving (Show, Read, Eq)

getUser :: Reminder -> User
getUser (Reminder user _) = user

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
toMicro (Years n) = n * (toMicro $ Months 12)
toMicro (Months n) = n * (toMicro $ Days 30)
toMicro (Days n) = n * (toMicro $ Hours 24)
toMicro (Hours n) = n * (toMicro $ Minutes 60)
toMicro (Minutes n) = n * (toMicro $ Seconds 60)
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
handleMessage reminds (AddTimeReminder reminder time) = do
    putStrLn "I will make sure the message is delivered"
    void (forkIO $ remind reminder time)
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
remind (Reminder user message) delayAmount = do
    unless (delayAmount == 0) $ delay delayAmount
    putStrLn $ user ++ " I was asked to remind you that \"" ++ message ++ "\""

printHelp :: User -> IO ()
printHelp nick = putStrLn $ unlines
    [ concat [nick, ": remind <user> in 200 seconds \"<message>\" - In 200 ",
        "seconds print <message> to <user>"]
    , concat [nick, ": remind <user> \"<message>\" - Next time <user> writes ",
        "a message print <message>"]
    , concat [nick, ": remind <user> in <amount> <unit> \"<message>\" - In ",
        "<amount> <unit> print <message> to <user> where unit is in [year, ",
        "month, day, hour, minute, second] user is either a nick or the ",
        "string \"me\" and amount is either \"a\", \"an\" or a number"]
    , concat [nick, ": remind help - Print this message."]
    ]

parseMessages :: User -> T.Text -> [Request]
parseMessages botnick =
    concatMap getRequests . mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    getRequests (ServerPrivMsg (IRCUser nick _ _) _ str) =
        case parse (parseRequest botnick (getNickname nick)) "(unknown)" str of
            Left _ -> [Message $ getNickname nick]
            Right request -> [Message (getNickname nick), request]
    getRequests _ = []

parseRequest :: User -> User -> GenParser Char st Request
parseRequest botnick nick = do
    void (string $ botnick ++ ": remind ")
    spaces

    parseHelp botnick <|> parseReminder nick

parseHelp :: User -> GenParser Char st Request
parseHelp botnick = do
    void $ try (string "help")
    spaces
    eof
    return $ Help botnick

parseReminder :: User -> GenParser Char st Request
parseReminder user = do
    toUser <- parseUser user
    spaces
    time <- optionMaybe parseTime
    spaces
    message <- parseMessage
    spaces
    eof

    case time of
        Just t -> return $ AddTimeReminder (Reminder toUser message) (toMicro t)
        Nothing -> return $ AddReminder (Reminder toUser message)

parseUser :: User -> GenParser Char st User
parseUser user = me user <|> otherUser

me :: User -> GenParser Char st User
me user = do
    void (try $ string "me ")
    return user

otherUser :: GenParser Char st User
otherUser = manyTill anyChar (try space)

parseTime :: GenParser Char st Time
parseTime = do
    void $ string "in "
    spaces
    amount <- parseAmount
    spaces

    parseUnit amount

parseAmount :: GenParser Char st Integer
parseAmount = parseNumber <|> parseLetterAmount

parseNumber :: GenParser Char st Integer
parseNumber = nat

parseLetterAmount :: GenParser Char st Integer
parseLetterAmount =
    try (string "an" >> return 1) <|> try (string "a" >> return 1)

parseUnit :: Integer -> GenParser Char st Time
parseUnit amount = try (string "years" >> return (Years amount)) <|>
    try (string "year" >> return (Years amount)) <|>
    try (string "months" >> return (Months amount)) <|>
    try (string "month" >> return (Months amount)) <|>
    try (string "days" >> return (Days amount)) <|>
    try (string "day" >> return (Days amount)) <|>
    try (string "hours" >> return (Hours amount)) <|>
    try (string "hour" >> return (Hours amount)) <|>
    try (string "minutes" >> return (Minutes amount)) <|>
    try (string "minute" >> return (Minutes amount)) <|>
    try (string "seconds" >> return (Seconds amount)) <|>
    try (string "second" >> return (Seconds amount))

parseMessage :: GenParser Char st Message
parseMessage = between (char '"') (char '"') (many (noneOf "\n\r\""))
