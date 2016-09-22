module Bot
  ( connect
  , disconnect
  , Bot
  , run
  , loop
  ) where

import Data.List
import Network
import System.IO
import System.Exit
import Control.Monad.Reader
import Control.Exception
import Text.Printf

server = "irc.freenode.org"
port = 6667
chan = "#dikufags"
nick = "dikunt"

type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }

disconnect :: Bot -> IO ()
disconnect = hClose . socket

connect :: IO Bot
connect = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)

loop :: Bot -> IO ()
loop st = runReaderT run st

run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick ++ " 0 * :tutorial bot")
    write "JOIN" chan
    asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    forever a = a >> forever a
    clean = drop 1 . dropWhile (/= ':') . drop 1
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

eval :: String -> Net ()
eval str
    | "dikunt: " `isPrefixOf` str = privmsg (drop 8 str)
    | otherwise = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

io :: IO a -> Net a
io = liftIO
