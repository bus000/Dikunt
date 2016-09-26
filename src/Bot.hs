module Bot
  ( connect
  , disconnect
  , Bot
  , run
  , loop
  ) where

import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
import Network
import System.IO
import Text.Printf

-- Bot modules
import BotTypes
    ( Net
    , BotFunction
    , Bot
    , socket
    , nickname
    , password
    , bot
    , channel
    , saveLastMsg
    , saveMsg
    )
import Functions.AsciiPicture (runAsciiPicture)
import Functions.Parrot (runParrot)
import Functions.WordReplacer (runReplaceWords)

port :: Integer
port = 6667

{- | List of all the crazy things Dikunt can do! The first of these actions to
 - return a value is chosen as the action for an incoming request. -}
functions :: [BotFunction]
functions = [runParrot, runAsciiPicture, runReplaceWords]

disconnect :: Bot -> IO ()
disconnect = hClose . socket

connect :: String -> String -> String -> String -> IO Bot
connect serv chan nick pass = do
    h <- connectTo serv (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return $ bot h nick chan pass

loop :: Bot -> IO ()
loop b = void $ runStateT run b

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
    saveMsg (clean s)
    if ping s then pong s else eval functions
    saveLastMsg s
  where
    clean = drop 1 . dropWhile (/= ':') . dropWhile (/= '#') . drop 1
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

eval :: [BotFunction] -> Net ()
eval fs = do
    results <- sequence fs
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
