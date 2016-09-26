module Bot
  ( connect
  , disconnect
  , Bot
  , run
  , loop
  ) where

import Control.Monad.State
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

type Net = StateT Bot IO
data Bot = Bot
  { socket      :: Handle
  , nickname    :: String
  , channel     :: String
  , password    :: String
  , lastMessage :: Maybe String
  }

saveLastMsg :: String -> Net ()
saveLastMsg msg = do
    st <- get
    let st' = st { lastMessage = Just msg }
    put st'

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
    return (Bot h nick chan pass Nothing) {- TODO: smart constructor. -}

loop :: Bot -> IO ()
loop bot = runStateT run bot >> return ()

run :: Net ()
run = do
    st <- get
    let pass = password st
        nick = nickname st
        chan = channel st

    write "NICK" nick
    write "USER" (nick ++ " 0 * :tutorial bot")
    write ("PRIVMSG NickServ : IDENTIFY " ++ nick) pass
    write "JOIN" chan
    listen

listen :: Net ()
listen = forever $ do
    st <- get
    let h = socket st
    s <- fmap init (liftIO $ hGetLine h)
    if ping s then pong s else eval (clean s) functions
    saveLastMsg s
  where
    clean = drop 1 . dropWhile (/= ':') . drop 1
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

eval :: String -> [BotFunction] -> Net ()
eval str fs = do
    results <- liftIO $ mapM (\x -> x str) fs
    case catMaybes results of
        (res:_) -> privmsg res
        _ -> return ()

privmsg :: String -> Net ()
privmsg s = do
    st <- get
    let chan = channel st

    let begin = chan ++ " :"
        ls = map (begin ++) (lines s)

    mapM_ (write "PRIVMSG") ls

write :: String -> String -> Net ()
write s t = do
    st <- get
    let h = socket st
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t
