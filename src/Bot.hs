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
import Control.Monad.Reader
import Text.Printf

server :: String
server = "irc.freenode.org"

port :: Integer
port = 6667

chan :: String
chan = "#dikufags"

nick :: String
nick = "dikunt"

type Net = ReaderT Bot IO
data Bot = Bot
  { socket :: Handle
  , password :: String
  }

disconnect :: Bot -> IO ()
disconnect = hClose . socket

connect :: String -> IO Bot
connect pass = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h pass)

loop :: Bot -> IO ()
loop st = runReaderT run st

run :: Net ()
run = do
    pass <- asks password
    write "NICK" nick
    write "USER" (nick ++ " 0 * :tutorial bot")
    write "PRIVMSG NickServ : IDENTIFY dikunt" pass
    write "JOIN" chan
    asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    clean = drop 1 . dropWhile (/= ':') . drop 1
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

eval :: String -> Net ()
eval str
    | "dikunt: " `isPrefixOf` str = privmsg (drop 8 str)
    | otherwise = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ (replaceOutput s))

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

replaceOutput :: String -> String
replaceOutput = unwords . map replace . words
  where
    replace "Mark" = "ShortGuy"
    replace "Jan" = "Tjekkeren"
    replace "Magnus" = "Glorious"
    replace "August" = "Motherless"
    replace "Oleks" = "Yoda"
    replace str = str

io :: IO a -> Net a
io = liftIO
