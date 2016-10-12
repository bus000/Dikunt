module Functions.Ponger
    ( ponger
    ) where

import qualified BotTypes as BT

ponger :: BT.BotFunction
ponger = BT.BotFunction
    { BT.shouldRun = shouldRun
    , BT.run = run
    , BT.help = "Pong's Ping messages."
    , BT.name = "Ponger"
    }

shouldRun :: BT.Message -> BT.Net Bool
shouldRun (BT.Ping _) = return True
shouldRun _ = return False

run :: BT.Message -> BT.Net [BT.Message]
run (BT.Ping from) = return $ [BT.Pong from]
run _ = fail "Pong should only respond to Ping's."
