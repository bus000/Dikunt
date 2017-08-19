module Types.Internal.MessageTest (tests) where

import Data.Aeson (encode, decode)
import Data.Maybe (isJust)
import Test.Tasty (TestTree , testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types.Internal.Message (Message, getMessage, message)

tests :: TestTree
tests = testGroup "Types.Internal.MessageTest"
    [ testProperty "jsonIdent" jsonIdent
    , testProperty "minSize" minSize
    , testProperty "getIdent" getIdent
    , testProperty "dontContain" dontContain
    , testProperty "combine" combine
    ]

jsonIdent :: Message -> Bool
jsonIdent msg = Just msg == (decode . encode) msg

minSize :: Message -> Bool
minSize msg = (length . getMessage) msg > 0

getIdent :: Message -> Bool
getIdent msg = Just msg == (message . getMessage) msg

dontContain :: Message -> Bool
dontContain msg = not $ any (`elem` "\0\r\n") (getMessage msg)

combine :: Message -> Message -> Bool
combine msg1 msg2 = isJust $ message (getMessage msg1 ++ getMessage msg2)
