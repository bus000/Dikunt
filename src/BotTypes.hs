module BotTypes
    ( Net
    , BotFunction
    , Bot
    , bot
    , saveLastMsg
    , getValue
    , socket
    , nickname
    , channel
    , password
    , message
    , lastMessage
    , saveMsg
    ) where

import Control.Monad.State
import System.IO

type Net = StateT Bot IO
data Bot = Bot
    { socket      :: Handle
    , nickname    :: String
    , channel     :: String
    , password    :: String
    , message     :: String
    , lastMessage :: Maybe String
    }

{- | Defines a Dikunt action. All new Dikunt features should implement a
 - function of this type and report in the functions list. -}
type BotFunction = Net (Maybe String)

bot :: Handle -> String -> String -> String -> Bot
bot h n c p = Bot h n c p "" Nothing

saveLastMsg :: String -> Net ()
saveLastMsg msg = do
    st <- get
    let st' = st { lastMessage = Just msg }
    put st'

saveMsg :: String -> Net ()
saveMsg msg = do
    st <- get
    let st' = st { message = msg }
    put st'

getValue :: (Bot -> a) -> Net a
getValue f = get >>= \st -> return $ f st
