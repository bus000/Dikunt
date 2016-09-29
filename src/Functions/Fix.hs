module Functions.Fix
    ( runFix
    ) where

import qualified Data.List as L
import qualified BotTypes as BT
import qualified Data.String.Utils as SU

fix :: String -> String -> String -> Maybe String
fix str lastmsg nick
    | (nick ++ ": fix ") `L.isPrefixOf` str = case words str' of
        [w1, "=", w2] -> Just $ SU.replace w1 w2 lastmsg
        _ -> Nothing
    | otherwise = Nothing
  where
    str' = drop (length $ nick ++ ": fix ") str

runFix :: BT.BotFunction
runFix msg = do
    {-let msg' = BT.messageString msg-}
    lastMsg <- BT.getValue BT.lastMessage
    nick <- BT.getValue BT.nickname

    case lastMsg of
        Nothing -> return Nothing
        Just l -> return $ fix (BT.messageString msg) (BT.messageString l) nick
