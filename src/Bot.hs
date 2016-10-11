module Bot
    ( connect
    , disconnect
    , Bot
    , loop
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.State
    ( liftIO
    , get
    , runStateT
    )
import Network
import System.IO
import Text.Printf
import Data.Time.Clock (DiffTime)

-- Bot modules
import BotTypes
    ( Net
    , BotFunction(..)
    , Bot
    , socket
    , nickname
    , password
    , bot
    , channel
    , Message(..)
    , message
    )
import Functions.AsciiPicture (asciiPicture)
import Functions.AsciiText (asciiText)
import Functions.Fix (fix)
import Functions.Parrot (parrot)
import Functions.Trump (trump)
import Functions.WordReplacer (wordReplacer)
import Functions.Greeting (greeting)

{- | List of all the crazy things Dikunt can do! The first of these actions to
 - return a value is chosen as the action for an incoming request. -}
functions :: [BotFunction]
functions =
    [ asciiPicture
    , asciiText
    , trump
    , fix
    , parrot
    , greeting
    , wordReplacer
    ]

disconnect :: Bot -> IO ()
disconnect = hClose . socket

connect :: String -> String -> String -> String -> Integer -> DiffTime -> IO Bot
connect serv chan nick pass port diff = do
    h <- connectTo serv (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return $ bot h nick chan pass diff

loop :: Bot -> IO ()
loop b = void $ runStateT runLoop b
  where
    runLoop = do
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
    case message s of
        Just m -> eval m functions
        Nothing -> return ()

eval :: Message -> [BotFunction] -> Net ()
eval (Ping from) _ = write "PONG" from
eval msg fs = do
    runables <- filterM (\f -> shouldRun f msg) fs
    case runables of
        (first:_) -> run first msg >>= privmsg
        [] -> return ()

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
