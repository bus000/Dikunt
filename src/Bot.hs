{- |
 - Module      : Bot
 - Description : Start, maintain and end connection to IRC channel.
 - Copyright   : (c) Magnus Stavngaard, 2016
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - Module exports 3 functions that are used to create, maintain and stop bots
 - running on IRC channels. The function connect takes configuration data and
 - sets up a connection to an IRC server. The function loop reads and parses
 - messages from the IRC server and pass them on to plugins handling the
 - messages. The function disconnect drops the connection to the IRC server.
 -}
module Bot
    ( connect
    , loop
    , disconnect
    ) where

import qualified BotTypes as BT
import Monitoring (Monitor(..), startMonitoring, inputHandle)
import Control.Concurrent (forkIO, readMVar, threadDelay)
import Control.Exception (catch, IOException)
import Control.Monad (forever)
import IRCParser.IRCMessageParser (parseMessage)
import IRCWriter.IRCWriter (writeMessage)
import Network (connectTo, PortID(..))
import Safe (readMay)
import System.IO
    ( hClose
    , hSetBuffering
    , hGetLine
    , BufferMode(..)
    , hPrint
    , Handle
    , hPutStrLn
    , stderr
    )
import Text.Printf (hPrintf)

{- | Connect the bot to an IRC server with the channel, nick, pass and port
 - given. Starts a monitor for the list of plugins given which will maintain a
 - running copy of each plugin. -}
connect :: BT.BotConfig
    -- ^ Configuration for bot.
    -> [FilePath]
    -- ^ Plugins to load.
    -> IO BT.Bot
connect (BT.BotConfig serv nick pass chan port) execs = do
    h <- connectTo serv (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering

    -- Start all plugins and a monitor for them.
    monitor <- startMonitoring execs [nick, chan]

    -- Create the bot.
    let bot = BT.bot h nick chan pass monitor

    -- Start thread reading from plugins and propagating messages to channel.
    _ <- forkIO $ respond bot

    return bot

{- | Register on channel with nickname and listen for messages from the server
 - the bot is connected to. The messages are parsed and passed to the list of
 - plugins in the bot. -}
loop :: BT.Bot
    -- ^ Bot to listen for messages from.
    -> IO ()
loop bot@(BT.Bot h nick chan pass _) = do
    write h $ BT.ClientNick nick
    write h $ BT.ClientUser nick 0 "DikuntBot"
    write h $ BT.ClientPrivMsg (BT.IRCUser "NickServ" Nothing Nothing)
        ("IDENTIFY " ++ nick ++ " " ++ pass)
    write h $ BT.ClientJoin [(chan, "")]

    listen bot

{- | Disconnect a bot from the server it is connected to. The functions should
 - be called when a bot is no longer used. -}
disconnect :: BT.Bot
    -- ^ Bot to disconnect.
    -> IO ()
disconnect = hClose . BT.socket

{- | Listen and handle messages from the IRC server. -}
listen :: BT.Bot
    -- ^ Bot to listen for messages for.
    -> IO ()
listen (BT.Bot h _ _ _ pluginHandles) = forever $ do
    s <- hGetLine h
    Monitor processes _ <- readMVar pluginHandles
    let ins = map inputHandle processes

    case parseMessage (s ++ "\n") of
        Just (BT.ServerPing from) -> write h $ BT.ClientPong from
        Just message -> mapM_ (safePrint message) ins
        Nothing -> hPutStrLn stderr $ "Could not parse message \"" ++ s ++ "\""
  where
    safePrint msg processHandle = catch (hPrint processHandle msg) (\e -> do
        let err = show (e :: IOException)
        hPutStrLn stderr err)

{- | Read from the output part of the pipe in the monitor and write messages
 - produced to the IRC channel. The function sleeps for 1 second after each
 - message write. If one sends messages to fast to IRC you are disconnected. -}
respond :: BT.Bot
    -- ^ Bot to respond for.
    -> IO ()
respond (BT.Bot h _ chan _ monitor) = forever $ do
    Monitor _ (output, _) <- readMVar monitor

    line <- hGetLine output
    case readMay line :: Maybe BT.ClientMessage of
        Just message -> write h message
        Nothing -> write h $ BT.ClientPrivMsgChan chan line

    threadDelay 1000000

{- | Write a clientmessage to a server handle. -}
write :: Handle
    -- ^ Server handle to write to.
    -> BT.ClientMessage
    -- ^ Message to write.
    -> IO ()
write h mes = hPrintf h $ writeMessage mes
