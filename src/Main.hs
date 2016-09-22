module Main (main) where

import Data.List
import Network
import System.Exit
import System.IO
import Text.Printf
import System.Environment
import System.Exit

server = "irc.freenode.org"
port = 6667
chan   = "#dikufags"
nick   = "dikunt"


main = do
    (password:args) <- getArgs
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick++" 0 * :tutorial bot")
    -- TODO: Verify identification
    write h "PRIVMSG NickServ : IDENTIFY dikunt" password
    write h "JOIN" chan
    writeMessage h "Debugging"
    listen h



write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

writeMessage :: Handle -> String -> IO ()
writeMessage h s = do
    hPrintf h "PRIVMSG %s : %s\r\n" chan s
    printf "PRIVMSG %s : %s\n" chan s

handleMessage :: Handle -> String -> IO ()
handleMessage h ('d':'i':'k':'u':'n':'t':':':' ':s) = do
    hPrintf h "PRIVMSG %s : %s\r\n" chan s
handleMessage h s = do
    print "fail"
    print s


listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t
    if ping s then pong s else handleMessage h (clean s)
    putStrLn s
  where
    forever a = a >> forever a

    clean     = drop 1 . dropWhile (/= ':') . drop 1

    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h "PONG" (':' : drop 6 x)
