module Bot
    ( connect
    , disconnect
    , loop
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (void, forever, filterM)
import Control.Monad.State (liftIO , runStateT)
import Data.Time.Clock (DiffTime)
import Network (connectTo, PortID(..))
import System.IO (hClose, hSetBuffering, hGetLine, BufferMode(..))
import Text.Printf (hPrintf)

-- Bot modules
import qualified BotTypes as BT
import qualified ExternalPlugins as EP
import Functions.AsciiPicture (asciiPicture)
import Functions.AsciiText (asciiText)
import Functions.BibleGem (biblegem)
import Functions.DanishMoan (danishMoan)
import Functions.Fix (fix)
import Functions.Greeting (greeting)
import Functions.Help (help)
import Functions.Insult (insult)
import Functions.News (news)
import Functions.Parrot (parrot)
import Functions.Ponger (ponger)
import Functions.Trump (trump)
import Functions.WordReplacer (wordReplacer)

disconnect :: BT.Bot -> IO ()
disconnect = hClose . BT.socket

connect :: String -> String -> String -> String -> Integer -> DiffTime -> IO BT.Bot
connect serv chan nick pass port diff = do
    h <- connectTo serv (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering

    externalFsMight <- EP.generatePlugins

    case externalFsMight of
        Nothing ->
            return $ BT.bot h nick chan pass diff internalFs
        Just externalFs ->
            return $ BT.bot h nick chan pass diff (externalFs ++ internalFs)
  where
    internalFs =
        [ ponger
        , asciiPicture
        , asciiText
        , trump
        , fix
        , help
        , news
        , insult
        , biblegem
        , danishMoan
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
eval msg fs = do
    runables <- filterM (`BT.shouldRun` msg) fs
    case runables of
        (first:_) -> BT.run first msg >>=
            mapM_ (\l -> write l >> liftIO (threadDelay 1000000))
        [] -> return ()

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
