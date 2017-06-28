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
{-# LANGUAGE OverloadedStrings #-}
module Bot
    ( connect
    , loop
    , disconnect
    ) where

import qualified BotTypes as BT
import Control.Concurrent (forkIO, readMVar, threadDelay)
import Control.Exception (catch, IOException)
import Control.Monad (forever, mapM_)
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import IRCParser.IRCMessageParser (parseMessage)
import IRCWriter.IRCWriter (writeMessage)
import Monitoring (Monitor(..), startMonitoring, inputHandle)
import Network (connectTo, PortID(..))
import System.IO (hClose, hSetBuffering, BufferMode(..), Handle, hPutStr, stdin, hFlush, stdout)
import qualified System.Log.Logger as Log
import qualified Data.ByteString.Lazy as B

{- | Connect the bot to an IRC server with the channel, nick, pass and port
 - given. Starts a monitor for the list of plugins given which will maintain a
 - running copy of each plugin. -}
connect :: BT.BotConfig
    -- ^ Configuration for bot.
    -> [FilePath]
    -- ^ Plugins to load.
    -> [String]
    -- ^ Extra plugin args.
    -> IO BT.Bot
connect (BT.BotConfig serv nick pass chan port) execs args = do
    h <- connectTo serv (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering

    -- Start all plugins and a monitor for them.
    monitor <- startMonitoring execs (nick:chan:args)

    -- Create the bot.
    let bot = BT.bot h nick chan pass monitor

    -- Start thread reading from plugins and propagating messages to channel.
    _ <- forkIO $ respond bot

    -- Start thread reading from stdin and propagating messages to channel.
    _ <- forkIO $ respondAdmin bot

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
listen bot@(BT.Bot h _ _ _ _) = do
    s <- B.hGetContents h
    mapM_ (handleMessage bot) $ messages s
  where
    messages str = map (\x -> x ++ "\n") $ lines (T.unpack . T.decodeUtf8 $ str)

{- | Handle a message from the IRC channel. If the message is a ping a pong
 - response is sent otherwise the message is sent to all plugins. The message is
 - also logged. -}
handleMessage :: BT.Bot
    -- ^ Bot that received the message.
    -> String
    -- ^ The message.
    -> IO ()
handleMessage (BT.Bot h _ _ _ pluginHandles) str = do
    Monitor processes _ <- readMVar pluginHandles
    let ins = map inputHandle processes
    case parseMessage str of
        Just (BT.ServerPing from) ->
            hPutStr h $ writeMessage (BT.ClientPong from)
        Just message -> do
            Log.infoM ("messages." ++ (show . BT.getServerCommand) message ++
                ".received") $ show message
            mapM_ (safePrint $ jsonEncode message) ins
        Nothing -> Log.errorM "bot.handleMessage" $
            "Could not parse message \"" ++ (init . init) str ++ "\""
  where
    safePrint msg processHandle = T.hPutStrLn processHandle msg `catch` (\e ->
        Log.errorM "bot.handleMessage" $ show (e :: IOException))

{- | Read from the output part of the pipe in the monitor and write messages
 - produced to the IRC channel. The function sleeps for 1 second after each
 - message write. If one sends messages to fast to IRC you are disconnected. -}
respond :: BT.Bot
    -- ^ Bot to respond for.
    -> IO ()
respond (BT.Bot h _ chan _ monitor) = forever $ do
    Monitor _ (output, _) <- readMVar monitor

    line <- T.hGetLine output
    case jsonDecode line of
        Just message -> write h message
        Nothing -> write h $ BT.ClientPrivMsgChan chan (T.unpack line)

    threadDelay 1000000

{- | Read from stdin and replicate the strings to the IRC channel. Can be used
 - by an administrator to write messages for Dikunt. -}
respondAdmin :: BT.Bot -> IO ()
respondAdmin (BT.Bot h _ chan _ _) = forever $ do
    putStr " > " >> hFlush stdout
    line <- T.hGetLine stdin
    case jsonDecode line of
        Just message -> write h message
        Nothing -> write h $ BT.ClientPrivMsgChan chan (T.unpack line)

{- | Write a clientmessage to a server handle. -}
write :: Handle
    -- ^ Server handle to write to.
    -> BT.ClientMessage
    -- ^ Message to write.
    -> IO ()
write h msg = do
    Log.infoM ("messages." ++ show command ++ ".send") $ show msg
    hPutStr h $ writeMessage msg
  where
    command = BT.getClientCommand msg

{- | Encode a value as JSON in a Lazy UTF8 text format. -}
jsonEncode :: ToJSON a => a
    -- ^ Value to encode.
    -> T.Text
jsonEncode = T.decodeUtf8 . encode

{- | Decode a value from a Lazy UTF8 text JSON. -}
jsonDecode :: FromJSON a => T.Text
    -- ^ Text to decode.
    -> Maybe a
jsonDecode = decode . T.encodeUtf8
