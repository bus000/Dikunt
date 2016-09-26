module Functions.Fix
    ( runFix
    ) where

import qualified Data.List as L
import qualified BotTypes as BT
import qualified Data.String.Utils as SU

fix :: String -> String -> Maybe String
fix str lastmsg
    | "dikunt: fix " `L.isPrefixOf` str = case words str' of
        [w1, "=", w2] -> Just $ SU.replace w1 w2 lastmsg
        _ -> Nothing
    | otherwise = Nothing
  where
    str' = drop (length "dikunt: fix ") str

runFix :: BT.BotFunction
runFix = do
    lastMsg <- BT.getValue BT.lastMessage
    msg <- BT.getValue BT.message

    case lastMsg of
        Nothing -> return Nothing
        Just l -> return $ fix msg l
