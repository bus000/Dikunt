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
import Functions.News (news)

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
        , news
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

        write $ BT.Nick nick
        write $ BT.User (nick ++ " 0 * :DikuntBot")
        write $ BT.PrivMsg nick "NickServ" ("IDENTIFY" ++ nick ++ pass)
        write $ BT.Join chan
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
eval (BT.Ping from) _ = write $ BT.Pong from
eval msg fs = do
    runables <- filterM (\f -> BT.shouldRun f msg) fs
    case runables of
        (first:_) -> BT.run first msg >>= privmsg
        [] -> return ()

{- TODO: remove. This function should not be needed. -}
privmsg :: String -> BT.Net ()
privmsg s = do
    chan <- BT.getValue BT.channel
    nick <- BT.getValue BT.nickname
    let msgs = map (BT.PrivMsg nick chan) (lines s)
    mapM_ (\l -> write l >> liftIO (threadDelay 1000000)) msgs

write :: BT.Message -> BT.Net ()
write message = BT.getValue BT.socket >>= \h -> liftIO $ write' h message
  where
    write' h (BT.PrivMsg _ to msg) =
        hPrintf h "PRIVMSG %s :%s\r\n" to msg
    write' h (BT.Pong to) =
        hPrintf h "PONG %s\r\n" to
    write' h (BT.Nick name) =
        hPrintf h "NICK %s\r\n" name
    write' h (BT.User name) =
        hPrintf h "USER %s\r\n" name
    write' h (BT.Join chan) =
        hPrintf h "JOIN %s\r\n" chan
    write' _ _ =
        putStrLn "Cannot send message type"
