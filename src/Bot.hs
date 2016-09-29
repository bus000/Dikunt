module Bot
  ( connect
  , disconnect
  , Bot
  , loop
  ) where

import Control.Concurrent
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
import Functions.Fix (runFix)
import Functions.Parrot (runParrot)
import Functions.WordReplacer (runReplaceWords)

{- | List of all the crazy things Dikunt can do! The first of these actions to
 - return a value is chosen as the action for an incoming request. -}
functions :: [BotFunction]
functions = [runAsciiPicture, runFix, runParrot, runReplaceWords]

disconnect :: Bot -> IO ()
disconnect = hClose . socket

connect :: String -> String -> String -> String -> Integer -> IO Bot
connect serv chan nick pass port = do
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
    saveMsg s
    if ping s then pong s else eval functions
    saveLastMsg (clean s)
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

eval :: [BotFunction] -> Net ()
eval fs = do
    message <- getValue message
    case message of
        Just message' -> do
            results <- sequence fs
            case catMaybes results of
                (res:_) -> privmsg res
                _ -> return ()
        Nothing -> return ()

privmsg :: String -> Net ()
privmsg s = do
    st <- get
    let chan = channel st

    let begin = chan ++ " :"
        ls = map (begin ++) (lines s)

    mapM_ writeLine ls
  where
    writeLine msg = do
        write "PRIVMSG" msg
        liftIO $ threadDelay 1000000

write :: String -> String -> Net ()
write s t = do
    st <- get
    let h = socket st
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t
