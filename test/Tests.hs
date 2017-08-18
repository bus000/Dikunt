module Tests where

import qualified Types.BotTypes as BT
import Data.Aeson (encode, decode)
import Test.Tasty
    ( TestTree
    , testGroup
    )
import Test.Tasty.HUnit
    ( testCase
    , (@=?)
    , Assertion
    , assertBool
    )
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "IRC Message Parser Tests"
    [ qcTests
    ]

qcTests :: TestTree
qcTests = testGroup "QuickCheck Tests"
    [ testProperty "jsonIRCUser" jsonIRCUser
    , testProperty "jsonServerMessage" jsonServerMessage
    , testProperty "jsonClientMessage" jsonClientMessage
    ]

jsonIRCUser :: BT.IRCUser -> Bool
jsonIRCUser user = Just user == (decode . encode) user

jsonServerMessage :: BT.ServerMessage -> Bool
jsonServerMessage message = Just message == (decode . encode) message

jsonClientMessage :: BT.ClientMessage -> Bool
jsonClientMessage message = Just message == (decode . encode) message
