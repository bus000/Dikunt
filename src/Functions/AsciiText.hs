module Functions.AsciiText
  ( asciiText
  ) where

import qualified BotTypes as BT
import Control.Monad.State (liftIO)
import Data.List (isPrefixOf)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)
import Data.Char (isSpace)

asciiText :: BT.BotFunction
asciiText = BT.BotFunction
    { BT.shouldRun = shouldRun
    , BT.run = run
    , BT.help = "asciitext: <text> - Outputs text as asciitext"
    , BT.name = "AsciiText"
    }

shouldRun :: BT.Message -> BT.Net Bool
shouldRun (BT.PrivMsg _ _ msg) =
    let msg' = dropWhile isSpace msg
    in return $ "asciitext:" `isPrefixOf` msg'
shouldRun _ = return False

run :: BT.Message -> BT.Net [BT.Message]
run (BT.PrivMsg _ _ msg) = do
    (e, s, _) <- liftIO $ readProcessWithExitCode "/usr/bin/toilet" [msg'] []
    case e of
        ExitSuccess -> BT.privmsgs s
        ExitFailure _ -> BT.privmsgs "Error in toilet"
  where
    msg' = dropWhile isSpace . drop (length "asciitext:") .
        dropWhile isSpace $ msg
run _ = fail "Asciitext should only run on PrivMsg's"
