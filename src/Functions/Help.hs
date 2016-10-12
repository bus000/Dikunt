module Functions.Help
    ( help
    ) where

import qualified BotTypes as BT

help :: BT.BotFunction
help = BT.BotFunction
    { BT.shouldRun = shouldRun
    , BT.run = run
    , BT.help = "<nick>: help <module> - Displays help information for module."
    , BT.name = "Help"
    }

shouldRun :: BT.Message -> BT.Net Bool
shouldRun (BT.PrivMsg _ _ msg) = do
    nick <- BT.getValue BT.nickname
    case words msg of
        [first, "help", _] -> return $ first == (nick ++ ":")
        [first, "help"] -> return $ first == (nick ++ ":")
        _ -> return False
shouldRun _ = return False

run :: BT.Message -> BT.Net [BT.Message]
run (BT.PrivMsg _ _ msg) = do
    functions <- BT.getValue BT.functions
    nick <- BT.getValue BT.nickname
    case words msg of
        [_, "help"] -> BT.privmsgs $ "Try " ++ nick ++ ": help <module>\n" ++
            modules functions
        [_, "help", myModule] -> helpModule myModule functions
        _ -> fail "Help does not match"
  where
    modules = concatMap (\f -> "    " ++ BT.name f ++ "\n")
    helpModule name functions =
        case filter (\f -> BT.name f == name) functions of
            (myModule:_) -> BT.privmsgs $ BT.help myModule
            _ -> BT.privmsgs "Could not find module."
run _ = fail "Help should only run on PrivMsg's"
