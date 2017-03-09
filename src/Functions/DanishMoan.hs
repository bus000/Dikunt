module Functions.DanishMoan
    ( danishMoan
    ) where

import qualified BotTypes as BT
import Safe (readMay)

danishMoan :: BT.BotFunction
danishMoan = BT.BotFunction
    { BT.shouldRun = shouldRun
    , BT.run = run
    , BT.help = "<nick>: danish moan n - Print a moan in Danish with n characters."
    , BT.name = "DanishMoan"
    }

shouldRun :: BT.Message -> BT.Net Bool
shouldRun (BT.PrivMsg _ _ msg) = do
    nick <- BT.getValue BT.nickname
    case words msg of
        [first, "danish", "moan", n] ->
            let a = first == (nick ++ ":")
                b = case readMay n :: Maybe Int of
                    Just n' -> n' > 0 && n' < 100
                    Nothing -> False
            in return $ a && b

run :: BT.Message -> BT.Net [BT.Message]
run (BT.PrivMsg _ _ msg) = case words msg of
    [first, "danish", "moan", n] -> case readMay n :: Maybe Int of
        Just n' -> BT.privmsgs $ ("Å" ++ replicate (n' - 1) 'å') ++ "hhhh"
        _ -> BT.privmsgs "Error"
    _ -> BT.privmsgs "Error"
