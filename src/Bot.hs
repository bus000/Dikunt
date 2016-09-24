module Bot
  ( connect
  , disconnect
  , Bot
  , run
  , loop
  ) where

import Data.Maybe
import Data.List
import Network
import System.IO
import Control.Monad.Reader
import Text.Printf

-- Bot modules
import Functions.AsciiPicture (runAsciiPicture)
import Functions.Parrot (parrot)

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

{- | Defines a Dikunt action. All new Dikunt features should implement a
 - function of this type and report in the functions list. -}
type BotFunction = String -> IO (Maybe String)

{- | List of all the crazy things Dikunt can do! The first of these actions to
 - return a value is chosen as the action for an incoming request. -}
functions :: [BotFunction]
functions = [parrot]

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
    replace "Oleks" = "Joleks"
    replace "10/10" = "knæhøj karse"
    replace "ha!" = "HAHAHAHAHAHHAHA!"
    replace str = str

io :: IO a -> Net a
io = liftIO
