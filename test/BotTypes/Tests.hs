module BotTypes.Tests (tests) where

import qualified BotTypes as BT

import Test.Tasty
    ( TestTree
    , testGroup
    )
import Test.Tasty.HUnit
    ( testCase
    , (@=?)
    , Assertion
    )
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Interpreter Tests"
    [ unitTests
    , qcTests
    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [ testCase "ircParse1" ircParse1
    , testCase "ircParse2" ircParse2
    , testCase "ircParse3" ircParse3
    , testCase "ircParse4" ircParse4
    , testCase "ircParse5" ircParse5
    ]

qcTests :: TestTree
qcTests = testGroup "QuickCheck Tests"
    [ -- testProperty "timesZero1" timesZero1
    ]


ircParse1 :: Assertion
ircParse1 = Just message @=? BT.message str
  where
    str = ":bus000_!~fluttersh@1.1.1.1 PRIVMSG #dikufags_test :my message"
    message = BT.PrivMsg "bus000_" "#dikufags_test" "my message"

ircParse2 :: Assertion
ircParse2 = Just message @=? BT.message str
  where
    str = "PING :wolfe.freenode.net"
    message = BT.Ping "wolfe.freenode.net"

ircParse3 :: Assertion
ircParse3 = Just message @=? BT.message str
  where
    str = ":bus000_!~fluttersh@1.1.1.1 JOIN #dikufags_test"
    message = BT.Join "bus000_"

ircParse4 :: Assertion
ircParse4 = Just message @=? BT.message str
  where
    str = ":bus000_!~fluttersh@1.1.1.1 PART #dikufags_test"
    message = BT.Part "bus000_"

ircParse5 :: Assertion
ircParse5 = Just message @=? BT.message str
  where
    str = ":wolfe.freenode.net NOTICE * :*** Looking up your hostname..."
    message = BT.Notice "wolfe.freenode.net" "*" "*** Looking up your hostname..."

{-ircParse6 :: Assertion-}
{-ircParse6 = Just message @=? parseIRCMessage str-}
  {-where-}
    {-str = ":bus000_!~fluttersh@1.1.1.1 PRIVMSG #dikufags_test :my message"-}
    {-message = BT.IRCMessage (Just prefix) BT.PRIVMSG params (Just trailing)-}
    {-prefix = BT.NicknamePrefix "bus000_" (Just "~fluttersh") (Just "1.1.1.1")-}
    {-params = ["#dikufags_test"]-}
    {-trailing = "my message"-}
