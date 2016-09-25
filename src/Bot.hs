module Bot
  ( connect
  , disconnect
  , Bot
  , run
  , loop
  ) where

import Control.Monad.Reader
import Data.List
import Data.Maybe
import Network
import System.IO
import Text.Printf

-- Bot modules
import Functions.AsciiPicture (runAsciiPicture)
import Functions.Parrot (parrot)
import Functions.WordReplacer (replaceWords)

port :: Integer
port = 6667

type Net = ReaderT Bot IO
data Bot = Bot
  { socket :: Handle
  , nickname :: String
  , channel  :: String
  , password :: String
  }

{- | Defines a Dikunt action. All new Dikunt features should implement a
 - function of this type and report in the functions list. -}
type BotFunction = String -> IO (Maybe String)

{- | List of all the crazy things Dikunt can do! The first of these actions to
 - return a value is chosen as the action for an incoming request. -}
functions :: [BotFunction]
functions = [parrot, replaceWords]

disconnect :: Bot -> IO ()
disconnect = hClose . socket

connect :: String -> String -> String -> String -> IO Bot
connect serv chan nick pass = do
    h <- connectTo serv (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h nick chan pass)

loop :: Bot -> IO ()
loop st = runReaderT run st

run :: Net ()
run = do
    pass <- asks password
    nick <- asks nickname
    chan <- asks channel
    write "NICK" nick
    write "USER" (nick ++ " 0 * :tutorial bot")
    write ("PRIVMSG NickServ : IDENTIFY "++nick) pass
    write "JOIN" chan
    asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s) functions
  where
    clean = drop 1 . dropWhile (/= ':') . drop 1
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

eval :: String -> [BotFunction] -> Net ()
eval str fs = do
    results <- io $ sequence (map (\x -> x str) fs)
    case catMaybes results of
        (res:_) -> privmsg res
        _ -> return ()

privmsg :: String -> Net ()
privmsg s = do
    chan <- asks channel

    let begin = chan ++ " :"
        ls = map (begin ++) (lines s)

    mapM_ (write "PRIVMSG") ls

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

io :: IO a -> Net a
io = liftIO
