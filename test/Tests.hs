module Tests where

import qualified BotTypes as BT
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
    [ unitTests
    , qcTests
    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [ jsonTests
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

jsonTests :: TestTree
jsonTests = testGroup "Layer 1 Tests"
    [ testCase "jsonIRCUser1" jsonIRCUser1
    , testCase "jsonIRCUser2" jsonIRCUser2
    , testCase "jsonIRCUser3" jsonIRCUser3
    , testCase "jsonIRCUser4" jsonIRCUser4
    ]

jsonIRCUser1 :: Assertion
jsonIRCUser1 = Just user @=? ((decode . encode) user :: Maybe BT.IRCUser)
  where
    user = BT.IRCUser "bus000" Nothing Nothing

jsonIRCUser2 :: Assertion
jsonIRCUser2 = Just user @=? ((decode . encode) user :: Maybe BT.IRCUser)
  where
    user = BT.IRCUser "☃" Nothing Nothing

jsonIRCUser3 :: Assertion
jsonIRCUser3 = Just user @=? ((decode . encode) user :: Maybe BT.IRCUser)
  where
    user = BT.IRCUser "Magnus" (Just "user") Nothing

jsonIRCUser4 :: Assertion
jsonIRCUser4 = Just user @=? ((decode . encode) user :: Maybe BT.IRCUser)
  where
    user = BT.IRCUser "Magnus" (Just "user") (Just "host")
