module Bot
    ( connect
    , disconnect
    , loop
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.State
    ( liftIO
    , runStateT
    )
import Network
import System.IO
import Text.Printf
import Data.Time.Clock (DiffTime)

-- Bot modules
import qualified BotTypes as BT
import Functions.AsciiPicture (asciiPicture)
import Functions.AsciiText (asciiText)
import Functions.Fix (fix)
import Functions.Parrot (parrot)
import Functions.Trump (trump)
import Functions.WordReplacer (wordReplacer)
import Functions.Greeting (greeting)
import Functions.Help (help)

disconnect :: BT.Bot -> IO ()
disconnect = hClose . BT.socket

connect :: String -> String -> String -> String -> Integer -> DiffTime -> IO BT.Bot
connect serv chan nick pass port diff = do
    h <- connectTo serv (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return $ BT.bot h nick chan pass diff fs
  where
    fs =
        [ asciiPicture
        , asciiText
        , trump
        , fix
        , help
        , parrot
        , greeting
        , wordReplacer
        ]

loop :: BT.Bot -> IO ()
loop b = void $ runStateT runLoop b
  where
    runLoop = do
        pass <- BT.getValue BT.password
        nick <- BT.getValue BT.nickname
        chan <- BT.getValue BT.channel

        write "NICK" nick
        write "USER" (nick ++ " 0 * :tutorial bot")
        write ("PRIVMSG NickServ : IDENTIFY " ++ nick) pass
        write "JOIN" chan
        listen

listen :: BT.Net ()
listen = forever $ do
    h <- BT.getValue BT.socket
    fs <- BT.getValue BT.functions
    s <- fmap init (liftIO $ hGetLine h)
    case BT.message s of
        Just m -> eval m fs
        Nothing -> return ()

eval :: BT.Message -> [BT.BotFunction] -> BT.Net ()
eval (BT.Ping from) _ = write "PONG" from
eval msg fs = do
    runables <- filterM (\f -> BT.shouldRun f msg) fs
    case runables of
        (first:_) -> BT.run first msg >>= privmsg
        [] -> return ()

privmsg :: String -> BT.Net ()
privmsg s = do
    chan <- BT.getValue BT.channel

    let begin = chan ++ " :"
        ls = map (begin ++) (lines s)

    mapM_ writeLine ls
  where
    writeLine msg = do
        write "PRIVMSG" msg
        liftIO $ threadDelay 1000000

write :: String -> String -> BT.Net ()
write s t = do
    h <- BT.getValue BT.socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t
