{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module ExternalPlugins
    ( generatePlugins
    ) where

import Control.Monad.State (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified BotTypes as BT
import GHC.Generics (Generic)
import System.Environment (setEnv)
import System.Process
import System.Exit

data Plugin = Plugin
    { help        :: String
    , name        :: String
    , programname :: String
    , command     :: String
    } deriving (Show, Eq, Generic)

instance FromJSON Plugin
instance ToJSON Plugin

generatePlugins :: IO (Maybe [BT.BotFunction])
generatePlugins = do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Plugin])
    case d of
        Left err -> putStrLn err >> return Nothing
        Right list -> return $ Just (map handlePlugin list)

handlePlugin :: Plugin -> BT.BotFunction
handlePlugin plugin = BT.BotFunction
    { BT.shouldRun = shouldRun (programname plugin)
    , BT.run = run (command plugin)
    , BT.help = (help plugin)
    , BT.name = (name plugin)
    }

shouldRun :: String -> (BT.Message -> BT.Net Bool)
shouldRun progname = shouldRun'
  where
    shouldRun' (BT.PrivMsg _ _ msg) = do
        nick <- BT.getValue BT.nickname
        case words msg of
            [first, second] ->
                return $ first == (nick ++ ":") && second == progname
            _ -> return False
    shouldRun' _ = return False

run :: String -> (BT.Message -> BT.Net [BT.Message])
run cmd = run'
  where
    run' msg = do
        liftIO $ setEnv "MESSAGE" (show msg)
        (e, s, _) <- liftIO $ readProcessWithExitCode cmd [] []
        case e of
            ExitSuccess -> BT.privmsgs s
            ExitFailure code ->
                BT.privmsgs $ "Process exited with error code " ++ (show code)

getJSON :: IO B.ByteString
getJSON = B.readFile pluginConfig

pluginConfig :: FilePath
pluginConfig = "./external/plugins.json"
