module Functions.BibleGem
    ( biblegem
    ) where

import Control.Monad.State (liftIO)
import qualified BotTypes as BT
import Network.Download (openURI)
import Data.ByteString.Char8 (unpack)
import Text.Regex.PCRE ((=~))

biblegem :: BT.BotFunction
biblegem = BT.BotFunction
    { BT.shouldRun = shouldRun
    , BT.run = run
    , BT.help = "<nick>: biblegem - Print a random verse from the bible."
    , BT.name = "BibleGem"
    }

shouldRun :: BT.Message -> BT.Net Bool
shouldRun (BT.PrivMsg _ _ msg) = do
    nick <- BT.getValue BT.nickname
    case words msg of
        [first, "biblegem"] -> return $ first == (nick ++ ":")
        _ -> return False
shouldRun _ = return False

run :: BT.Message -> BT.Net String
run _ = do
    res <- liftIO (openURI randomQuote)
    case res of
        Left err -> return err
        Right passage -> return $ format (unpack passage)

format :: String -> String
format passage = verse ++ ": " ++ content
  where
    pattern = "^<b>(.*)<\\/b> (.*)$"
    [[verse, content]] = passage =~ pattern :: [[String]]

randomQuote :: String
randomQuote = "http://labs.bible.org/api/?passage=random"
