module Functions.Greeting
    ( greeting
    ) where

import qualified BotTypes as BT
import Data.Time.Clock
    ( getCurrentTime
    , utctDayTime
    , utctDayTime
    , DiffTime
    , diffTimeToPicoseconds
    )
import Control.Monad.State (liftIO)

greeting :: BT.BotFunction
greeting = BT.BotFunction
    { BT.shouldRun = shouldRun
    , BT.run = run
    , BT.help = "Greets people when they join and leave"
    , BT.name = "Greeting"
    }

shouldRun :: BT.Message -> BT.Net Bool
shouldRun (BT.Join _) = return True
shouldRun (BT.Quit _) = return True
shouldRun (BT.Part _) = return True
shouldRun _ = return False

run :: BT.Message -> BT.Net String
run (BT.Join nick) = do
    offSet <- BT.getValue BT.timeOffset
    hour <- liftIO $ hourOfDay offSet

    case timeOfDay hour of
        Just Morning -> return $ "Good morning " ++ nick
        Just AfterNoon -> return $ "Good afternoon " ++ nick
        Just Evening -> return $ "Good evening " ++ nick
        Just Night -> return $ "Good night " ++ nick
        Nothing -> return $ "Hello and welcome " ++ nick
run (BT.Quit nick) = return $ "Goodbye " ++ nick
run (BT.Part nick) = return $ "Goodbye " ++ nick
run _ = fail "Should only run on Join's and Quit's."

hourOfDay :: DiffTime -> IO Integer
hourOfDay offset = do
    time <- liftIO getCurrentTime
    let localTime = utctDayTime time + offset
    return $ quot (diffTimeToPicoseconds localTime) (36 * 10^(14 :: Integer))

data TimeOfDay = Night | Morning | AfterNoon | Evening

timeOfDay :: Integer -> Maybe TimeOfDay
timeOfDay i
    | i >= 0 && i <= 6 = Just Night
    | i > 6 && i <= 12 = Just Morning
    | i > 12 && i <= 18 = Just AfterNoon
    | i > 18 && i < 12 = Just Evening
    | otherwise = Nothing
