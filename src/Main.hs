{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Bot (connect, disconnect, loop)
import Control.Exception (bracket)
import System.Console.CmdArgs
    ( Data
    , Typeable
    , helpArg
    , explicit
    , name
    , (&=)
    , help
    , versionArg
    , def
    , summary
    , cmdArgs
    )
import Control.Monad
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Safe (readMay)
import System.Directory

data Dikunt = Dikunt
    { server     :: String
    , nickname   :: String
    , password   :: String
    , channel    :: String
    , port       :: Integer
    , timeOffset :: String
    } deriving (Data, Typeable, Show, Eq)

dikunt :: Dikunt
dikunt = Dikunt
    { server = "irc.freenode.org" &= help "Server to connect to"
    , nickname = "dikunt" &= help "Nick to use"
    , password = def &= help "Password to use"
    , channel = "#dikufags" &= help "Channel to connect to"
    , port = 6667 &= help "Port to connect to"
    , timeOffset = "0" &= help "UTC time offset"
    } &=
        help "Bot to run on IRC channels" &=
        summary "Dikunt v0.0.0.0 (C) Magnus Stavngaard" &=
        helpArg [explicit, name "h"] &=
        versionArg [explicit, name "v"]

mode :: IO Dikunt
mode = cmdArgs dikunt

main :: IO ()
main = do
    arguments <- mode
    -- TODO: Make type representing these bot parameters.
    let serv = server arguments
        pass = password arguments
        nick = nickname arguments
        chan = channel arguments
        port' = port arguments
        offset = timeOffset arguments

    executables <- getExecutables "./plugins/"

    case computeOffset offset of
        Just off ->
            bracket (connect serv chan nick pass port' off executables) disconnect loop
        Nothing ->
            putStrLn $ "Could not parse UTC offset " ++ offset

getExecutables :: FilePath -> IO [FilePath]
getExecutables dir = do
    files <- getDirectoryContents dir
    filterM (liftM executable . getPermissions) (map (\x -> dir ++ x) files)

computeOffset :: String -> Maybe DiffTime
computeOffset ('+':int) = do
    off <- readMay int
    if off > 0 && off < 15
    then return $ secondsToDiffTime (off * 3600)
    else Nothing
computeOffset ('-':int) = do
    off <- readMay int
    if off > 0 && off < 13
    then return $ secondsToDiffTime (-(off * 3600))
    else Nothing
computeOffset ['0'] = Just $ secondsToDiffTime 0
computeOffset _ = Nothing
