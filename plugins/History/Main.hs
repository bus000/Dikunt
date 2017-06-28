module Main (main) where

import qualified BotTypes as BT
import Control.Monad (void)
import Data.Aeson (decode)
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Paths_Dikunt (getDataFileName)
import Safe (readMay)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..), readFile)
import Text.ParserCombinators.ReadP
    ( ReadP
    , eof
    , readP_to_S
    , skipSpaces
    , (+++)
    , string
    , char
    , munch
    , munch1
    , pfail
    , between
    , many
    , satisfy
    , look
    )

data Request = HelpRequest | GetRequest Position Position

data Position = Constant Int
    | Last
    | First
    | Sub Position Position
    | Add Position Position
  deriving (Eq, Show, Read)

main :: IO ()
main = do
    (nick:_) <- getArgs

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    privmsgs <- getDataFileName "data/privmsgLog.db"
    lineN <- length . lines <$> readFile privmsgs

    messages <- parseMessages nick <$> T.hGetContents stdin

    mapM_ (handleMessage nick privmsgs) $ zip [lineN..] messages

handleMessage :: BT.Nickname -> FilePath -> (Int, Maybe Request) -> IO ()
handleMessage _ _ (_, Nothing) = return ()
handleMessage nick _ (_, Just HelpRequest) = putStrLn $ helpString nick
handleMessage _ file (messageNumber, Just (GetRequest startPos endPos)) =
    case evaluatePositions messageNumber startPos endPos of
        Right (start, end) -> do
            str <- parseLines <$> getLines file start end
            putStrLn str
        Left err -> putStrLn err

parseLines :: [String] -> String
parseLines = unlines . map parseLine
  where
    parseLine line = case readP_to_S parser line of
        [(content, "")] -> content
        x -> show x -- TODO: should return nothing.

    parser = do
        time <- between (char '[') (char ']') parseTime
        void $ char ' '
        (user, message) <- parseServer +++ parseClient
        eof
        return $ time ++ " " ++ user ++ ": " ++ message

    parseTime = do
        yearMonthDay <- munch1 ((/=) ' ')
        hourMinutSecond <- munch1 ((/=) ' ')
        void (many $ satisfy (\_ -> True))
        return $ yearMonthDay ++ " " ++ hourMinutSecond ++ " EDT"

    parseServer = do
        input <- look
        case readMay input of
            Just (BT.ServerPrivMsg (BT.IRCUser user _ _) _ message) ->
                return (user, message)
            _ -> pfail

    parseClient = do
        input <- look
        case readMay input of
            Just (BT.ClientPrivMsg (BT.IRCUser _ _ _) _) ->
                return ("dikunt", "Redacted")
            Just (BT.ClientPrivMsgChan _ message) ->
                return ("dikunt", message)
            _ -> pfail

getLines :: String -> Int -> Int -> IO [String]
getLines file start end = take (end - start) . drop start . lines <$> readFile file

evaluatePositions :: Int -> Position -> Position -> Either String (Int, Int)
evaluatePositions lastline pos1 pos2
    | start > lastline || end > lastline =
        Left "Error: Start or end position is greater than the number of lines"
    | end - start > 10 = Left "Error: To large interval"
    | start == end = Left "Error: To small interval"
    | otherwise = return (start, end)

  where
    pos1' = evaluate pos1
    pos2' = evaluate pos2

    start = min pos1' pos2'
    end = max pos1' pos2'

    evaluate First = 0
    evaluate Last = lastline
    evaluate (Constant n) = n
    evaluate (Sub e1 e2) =
        let v1 = evaluate e1
            v2 = evaluate e2
        in v1 - v2
    evaluate (Add e1 e2) =
        let v1 = evaluate e1
            v2 = evaluate e2
        in v1 + v2

helpString :: String -> String
helpString nick = unlines
    [ nick ++ ": history help - Display this message."
    , nick ++ ": history get <position>:<position> - Get messages between the two positions."
    , "    where <position> is produced by the grammar"
    , "    S -> P~P | P+P | P"
    , "    P -> FIRST | LAST | N"
    , "    N -> integer literal"
    ]

parseMessages :: BT.Nickname -> T.Text -> [Maybe Request]
parseMessages nick = map (parseRequest nick) . mapMaybe getMessage .
    mapMaybe (decode . T.encodeUtf8) . T.lines
  where
    getMessage (BT.ServerPrivMsg _ _ str) = Just str
    getMessage _ = Nothing

parseRequest :: BT.Nickname -> String -> Maybe Request
parseRequest nick str = case readP_to_S parseRequest' str of
    [(result, "")] -> Just result
    _ -> Nothing
  where
    parseRequest' = parseHelpRequest nick +++ parseGetRequest nick

parseHelpRequest :: BT.Nickname -> ReadP Request
parseHelpRequest nick = do
    skipSpaces
    void $ string (nick ++ ":")
    skipSpaces
    void $ string "history"
    skipSpaces
    void $ string "help"
    skipSpaces
    eof

    return HelpRequest

parseGetRequest :: BT.Nickname -> ReadP Request
parseGetRequest nick = do
    skipSpaces
    void $ string (nick ++ ":")
    skipSpaces
    void $ string "history"
    skipSpaces
    void $ string "get"
    skipSpaces
    startPos <- parsePosition
    skipSpaces
    void $ char ':'
    skipSpaces
    endPos <- parsePosition
    skipSpaces
    eof

    return $ GetRequest startPos endPos

parsePosition :: ReadP Position
parsePosition = parseFinal +++ parseSub +++ parseAdd

parseSub :: ReadP Position
parseSub = do
    a <- parseFinal
    void $ char '~'
    b <- parsePosition

    return $ Sub a b

parseAdd :: ReadP Position
parseAdd = do
    a <- parseFinal
    void $ char '+'
    b <- parsePosition

    return $ Add a b

parseFinal :: ReadP Position
parseFinal = parseConstant +++ parseFirst +++ parseLast

parseConstant :: ReadP Position
parseConstant = do
    n <- read <$> munch (`elem` ['0'..'9'])
    return $ Constant n

parseFirst :: ReadP Position
parseFirst = do
    void $ string "FIRST"
    return First

parseLast :: ReadP Position
parseLast = do
    void $ string "LAST"
    return Last
