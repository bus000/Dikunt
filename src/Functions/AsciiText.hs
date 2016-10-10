module Functions.AsciiText
  ( asciiText
  ) where

import qualified BotTypes as BT
import Control.Monad.State
import Data.List
import System.Exit
import System.Process
import Data.Char (isSpace)

asciiText :: BT.BotFunction
asciiText = BT.BotFunction
    { BT.shouldRun = asciiTextShouldRun
    , BT.run = asciiTextRun
    , BT.help = "asciitext: <text> - Outputs text as asciitext"
    , BT.name = "AsciiText"
    }

asciiTextShouldRun :: BT.Message -> BT.Net Bool
asciiTextShouldRun (BT.PrivMsg _ _ msg) =
    let msg' = dropWhile isSpace msg
    in return $ "asciitext:" `isPrefixOf` msg'
asciiTextShouldRun _ = return False

asciiTextRun :: BT.Message -> BT.Net String
asciiTextRun (BT.PrivMsg _ _ msg) = do
    (e, s, _) <- liftIO $ readProcessWithExitCode "/usr/bin/toilet" [msg'] []
    case e of
        ExitSuccess -> return s
        ExitFailure _ -> return "Error in toilet"
  where
    msg' = (dropWhile isSpace) . (drop $ length "asciitext:") .
        (dropWhile isSpace) $ msg
asciiTextRun _ = return "Asciitext should only run on PrivMsg's"
