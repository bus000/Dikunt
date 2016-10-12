module Functions.Fix
    ( fix
    ) where

import qualified BotTypes as BT

fix :: BT.BotFunction
fix = BT.BotFunction
    { BT.shouldRun = shouldRun
    , BT.run = run
    , BT.help = "not ready"
    , BT.name = "Fix"
    }

shouldRun :: BT.Message -> BT.Net Bool
shouldRun _ = return False

run :: BT.Message -> BT.Net [BT.Message]
run = undefined
