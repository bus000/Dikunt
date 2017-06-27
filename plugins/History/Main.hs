module Main (main) where

import qualified BotTypes as BT
import Control.Monad (foldM_)
import Data.Aeson (decode)
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Prelude hiding (lines)
import System.Environment (getArgs)
import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))
import Text.ParserCombinators.ReadP
    ( ReadP
    , eof
    , readP_to_S
    , skipSpaces
    , (+++)
    , string
    , char
    , munch
    )

type State = Int

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

    messages <- parseMessages nick <$> T.hGetContents stdin

    -- TODO: should not be 0.
    foldM_ (handleMessage nick) 0 messages

handleMessage :: BT.Nickname -> State -> Maybe Request -> IO State
handleMessage _ messageNumber Nothing = return $ messageNumber + 1
handleMessage nick messageNumber (Just HelpRequest) = do
    putStrLn $ helpString nick
    return $ messageNumber + 1
handleMessage _ messageNumber (Just (GetRequest startPos endPos))
    | start > messageNumber || end > messageNumber = do
        putStrLn "Error: Start or end position is greater than the number of lines"
        return $ messageNumber + 1
    | abs (start - end) > 10 = do
        putStrLn "Error: To large interval"
        return $ messageNumber + 1
    | start == end = do
        putStrLn "Error: To small interval"
        return $ messageNumber + 1
    | start < end = do
        ls <- getLines start end
        putStrLn $ unlines ls
        return $ messageNumber + 1
    | start > end = do
        ls <- getLines end start
        putStrLn $ unlines (reverse ls)
        return $ messageNumber + 1
    | otherwise = return $ messageNumber + 1
  where
    start = evaluate messageNumber startPos
    end = evaluate messageNumber endPos

evaluate :: Int -> Position -> Int
evaluate _ First = 0
evaluate lastline Last = lastline
evaluate _ (Constant n) = n
evaluate lastline (Sub e1 e2) =
    let v1 = evaluate lastline e1
        v2 = evaluate lastline e2
    in v1 - v2
evaluate lastline (Add e1 e2) =
    let v1 = evaluate lastline e1
        v2 = evaluate lastline e2
    in v1 + v2

getLines :: Int -> Int -> IO [String]
getLines _ _ = return ["hej", "med", "dig"]

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
    _ <- string $ nick ++ ":"
    skipSpaces
    _ <- string "history"
    skipSpaces
    _ <- string "help"
    skipSpaces
    eof

    return HelpRequest

parseGetRequest :: BT.Nickname -> ReadP Request
parseGetRequest nick = do
    skipSpaces
    _ <- string $ nick ++ ":"
    skipSpaces
    _ <- string "history"
    skipSpaces
    _ <- string "get"
    skipSpaces
    startPos <- parsePosition
    skipSpaces
    _ <- char ':'
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
    _ <- char '~'
    b <- parsePosition

    return $ Sub a b

parseAdd :: ReadP Position
parseAdd = do
    a <- parseFinal
    _ <- char '+'
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
    _ <- string "FIRST"
    return First

parseLast :: ReadP Position
parseLast = do
    _ <- string "LAST"
    return Last
