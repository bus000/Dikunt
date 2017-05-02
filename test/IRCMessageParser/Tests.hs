module IRCMessageParser.Tests where

import qualified BotTypes as BT
import Data.List (nub)
import IRCParser.Layer1Impl
import IRCParser.Layer2Impl
import Text.ParserCombinators.ReadP (readP_to_S)

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
    [ layer1UnitTests
    , layer2UnitTests
    ]

layer1UnitTests :: TestTree
layer1UnitTests = testGroup "Layer 1 Tests"
    [
    {- User tests. -}
      testCase "parseUser1" parseUser1
    , testCase "parseUser2" parseUser2
    , testCase "parseUser3" parseUser3

    {- Nickname tests. -}
    , testCase "parseNickname1" parseNickname1
    , testCase "parseNickname2" parseNickname2

    {- IPv6 tests. -}
    , testCase "parseIP6addr1" parseIP6addr1
    , testCase "parseIP6addr2" parseIP6addr2
    , testCase "parseIP6addr3" parseIP6addr3
    , testCase "parseIP6addr4" parseIP6addr4
    , testCase "parseIP6addr5" parseIP6addr5

    {- IPv4 tests. -}
    , testCase "parseIP4addr1" parseIP4addr1
    , testCase "parseIP4addr2" parseIP4addr2

    {- Hostaddress tests. -}
    , testCase "parseHostaddr1" parseHostaddr1
    , testCase "parseHostaddr2" parseHostaddr2

    {- Shortname tests. -}
    , testCase "parseShortname1" parseShortname1
    , testCase "parseShortname2" parseShortname2
    , testCase "parseShortname3" parseShortname3

    {- Hostname tests. -}
    , testCase "parseHostname1" parseHostname1
    , testCase "parseHostname2" parseHostname2
    , testCase "parseHostname3" parseHostname3

    {- Host tests. -}
    , testCase "parseHost1" parseHost1
    , testCase "parseHost2" parseHost2

    {- Nickname prefix tests. -}
    , testCase "parseNicknamePrefix1" parseNicknamePrefix1
    , testCase "parseNicknamePrefix2" parseNicknamePrefix2
    , testCase "parseNicknamePrefix3" parseNicknamePrefix3

    {- Servername prefix tests. -}
    , testCase "parseServername1" parseServername1
    , testCase "parseServername2" parseServername2

    {- Trailing tests. -}
    , testCase "parseTrailing1" parseTrailing1

    {- Middle tests. -}
    , testCase "parseMiddle1" parseMiddle1
    , testCase "parseMiddle2" parseMiddle2

    {- Parameters tests. -}
    , testCase "parseParams1" parseParams1
    , testCase "parseParams2" parseParams2
    , testCase "parseParams3" parseParams3
    , testCase "parseParams4" parseParams4

    {- Command tests. -}
    , testCase "parseCommand1" parseCommand1
    , testCase "parseCommand2" parseCommand2
    , testCase "parseCommand3" parseCommand3
    , testCase "parseCommand4" parseCommand4
    , testCase "parseCommand5" parseCommand5

    {- Message tests. -}
    , testCase "parseMessage1" parseMessage1
    , testCase "parseMessage2" parseMessage2
    , testCase "parseMessage3" parseMessage3
    , testCase "parseMessage4" parseMessage4
    , testCase "parseMessage5" parseMessage5
    , testCase "parseMessage6" parseMessage6
    ]

layer2UnitTests :: TestTree
layer2UnitTests = testGroup "Layer 2 Tests"
    [
    ]

qcTests :: TestTree
qcTests = testGroup "QuickCheck Tests"
    [ testProperty "alwaysTerminate" alwaysTerminate
    ]

alwaysTerminate :: String -> Bool
alwaysTerminate s = case parseIRCMessage s of
    _ -> True

parseUser1 :: Assertion
parseUser1 = readP_to_S parseUser str @=? [("~fluttersh", "@1.1.1.1")]
  where str = "~fluttersh@1.1.1.1"

parseUser2 :: Assertion
parseUser2 = readP_to_S parseUser str @=? [("fluttershy", " PRIVMSG (...)")]
  where str = "fluttershy PRIVMSG (...)"

parseUser3 :: Assertion
parseUser3 = readP_to_S parseUser str @=? []
  where str = "@1.1.1.1"

parseNickname1 :: Assertion
parseNickname1 = assertBool "" $ ("bus000_", "!~fluttersh@") `elem`
    readP_to_S parseNickname str
  where str = "bus000_!~fluttersh@"

parseNickname2 :: Assertion
parseNickname2 = assertBool "" (not $ ("123456789", "") `elem`
    readP_to_S parseNickname str)
  where str = "123456789"

parseIP6addr1 :: Assertion
parseIP6addr1 = readP_to_S parseIP6addr str @=?
    [("2001:DB8:A0B:12F0:0:0:0:1", "")]
  where str = "2001:DB8:A0B:12F0:0:0:0:1"

parseIP6addr2 :: Assertion
parseIP6addr2 = readP_to_S parseIP6addr str @=? []
  where str = "2001:DB8:A0B:12F0:0:0:1"

parseIP6addr3 :: Assertion
parseIP6addr3 = readP_to_S parseIP6addr str @=?
    [("2001:DB8:A0B:12F0:0:0:0:1", " PRIVMSG (...)")]
  where str = "2001:DB8:A0B:12F0:0:0:0:1 PRIVMSG (...)"

parseIP6addr4 :: Assertion
parseIP6addr4 = readP_to_S parseIP6addr str @=?
    [("0:0:0:0:0:0:127.0.0.1", "")]
  where str = "0:0:0:0:0:0:127.0.0.1"

parseIP6addr5 :: Assertion
parseIP6addr5 = readP_to_S parseIP6addr str @=? [("1:2:3:4:5:6:7:8", "")]
  where str = "1:2:3:4:5:6:7:8"

parseIP4addr1 :: Assertion
parseIP4addr1 = readP_to_S parseIP4addr str @=? [("192.168.1.6", " PRIVMSG ..")]
  where str = "192.168.1.6 PRIVMSG .."

parseIP4addr2 :: Assertion
parseIP4addr2 = readP_to_S parseIP4addr str @=? [("192.168.1.6", ".123")]
  where str = "192.168.1.6.123"

parseHostaddr1 :: Assertion
parseHostaddr1 = readP_to_S parseHostaddr str @=?
    [("0:0:0:0:0:0:127.0.0.1", "")]
  where str = "0:0:0:0:0:0:127.0.0.1"

parseHostaddr2 :: Assertion
parseHostaddr2 = readP_to_S parseHostaddr str @=?  []
  where str = "0:0:0:0:0:127.0.0.1"

parseShortname1 :: Assertion
parseShortname1 = assertBool "" $ ("bus000-asdf", " PRIV") `elem`
    readP_to_S parseShortname str
  where str = "bus000-asdf PRIV"

parseShortname2 :: Assertion
parseShortname2 = assertBool "" $ ("wolfe", ".freenode.net NOTICE *:123") `elem`
    readP_to_S parseShortname str
  where str = "wolfe.freenode.net NOTICE *:123"

parseShortname3 :: Assertion
parseShortname3 = [("a", "")] @=? readP_to_S parseShortname "a"

parseHostname1 :: Assertion
parseHostname1 = assertBool "" $ ("wolfe.freenode.net", " NOTICE *:123") `elem`
    readP_to_S parseHostname str
  where str = "wolfe.freenode.net NOTICE *:123"

parseHostname2 :: Assertion
parseHostname2 = assertBool "" $ ("wolfe.free-node.net", " NOTICE *:123") `elem`
    readP_to_S parseHostname str
  where str = "wolfe.free-node.net NOTICE *:123"

parseHostname3 :: Assertion
parseHostname3 = readP_to_S parseHostname str @=? []
  where str = ".wolfe.free-node.net NOTICE *:123"

parseHost1 :: Assertion
parseHost1 = assertBool "" $ ("wolfe.freenode.net", " NOTICE *:123") `elem`
    readP_to_S parseHost str
  where str = "wolfe.freenode.net NOTICE *:123"

parseHost2 :: Assertion
parseHost2 = assertBool "" $ ("0:0:0:0:0:0:127.0.0.1", "") `elem`
    readP_to_S parseHost str
  where str = "0:0:0:0:0:0:127.0.0.1"

parseNicknamePrefix1 :: Assertion
parseNicknamePrefix1 = assertBool "" $ (expected, rest) `elem` output
  where
    str = "bus000_!~fluttersh@1.1.1.1 PRIVMSG"
    expected = BT.NicknamePrefix
        (BT.IRCUser "bus000_" (Just "~fluttersh") (Just "1.1.1.1"))
    rest = " PRIVMSG"
    output = readP_to_S parseNicknamePrefix str

parseNicknamePrefix2 :: Assertion
parseNicknamePrefix2 = assertBool "" $ (expected, rest) `elem` output
  where
    str = "bus000_!~fluttersh PRIVMSG"
    expected = BT.NicknamePrefix
        (BT.IRCUser "bus000_" (Just "~fluttersh") Nothing)
    rest = " PRIVMSG"
    output = readP_to_S parseNicknamePrefix str

parseNicknamePrefix3 :: Assertion
parseNicknamePrefix3 = assertBool "" $ (expected, rest) `elem` output
  where
    str = "bus000_ PRIVMSG"
    expected = BT.NicknamePrefix
        (BT.IRCUser "bus000_" Nothing Nothing)
    rest = " PRIVMSG"
    output = readP_to_S parseNicknamePrefix str

parseServername1 :: Assertion
parseServername1 = assertBool "" $ (expected, rest) `elem` output
  where
    str = "wolfe.freenode.net NOTICE * :trailing"
    expected = BT.ServernamePrefix "wolfe.freenode.net"
    rest = " NOTICE * :trailing"
    output = readP_to_S parseServername str

parseServername2 :: Assertion
parseServername2 = assertBool "" $ (expected, rest) `elem` output
  where
    str = "wolfe.free-node.net NOTICE * :trailing"
    expected = BT.ServernamePrefix "wolfe.free-node.net"
    rest = " NOTICE * :trailing"
    output = readP_to_S parseServername str

parseTrailing1 :: Assertion
parseTrailing1 = assertBool "" $ (expected, rest) `elem` output
  where
    str = ": this is a trailing message \r\n"
    expected = ": this is a trailing message "
    rest = "\r\n"
    output = readP_to_S parseTrailing str

parseMiddle1 :: Assertion
parseMiddle1 = assertBool "" $ (expected, rest) `elem` output
  where
    str = "parameter123 :trailing message\r\n"
    expected = "parameter123"
    rest = " :trailing message\r\n"
    output = readP_to_S parseMiddle str

parseMiddle2 :: Assertion
parseMiddle2 = [] @=? output
  where
    str = ":parameter123 :trailing message\r\n"
    output = readP_to_S parseMiddle str

parseParams1 :: Assertion
parseParams1 = assertBool "" $ (expected, rest) `elem` output
  where
    str = " parameter1~ parameter2! parameter3` parameter4: :trailing mes\r\n"
    expected = (["parameter1~", "parameter2!", "parameter3`", "parameter4:"],
        Just "trailing mes")
    rest = "\r\n"
    output = readP_to_S parseParams str

parseParams2 :: Assertion
parseParams2 = [(([], Nothing), str)] @=? output
  where
    str = "param :trailing\r\n"
    output = readP_to_S parseParams str

parseParams3 :: Assertion
parseParams3 = assertBool "" $ (expected, rest) `elem` output
  where
    str = " 1 2 3 4 5 6 7 8 9 10 11 12 13 14 trailing message\r\n"
    expected = (["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
        "13", "14"], Just "trailing message")
    rest = "\r\n"
    output = readP_to_S parseParams str

parseParams4 :: Assertion
parseParams4 = assertBool "" $ (expected, rest) `elem` output
  where
    str = " 1 2 3 4 5 6 7 8 9 10 11 12 13 14 :trailing message\r\n"
    expected = (["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
        "13", "14"], Just "trailing message")
    rest = "\r\n"
    output = readP_to_S parseParams str

parseCommand1 :: Assertion
parseCommand1 = [(expected, rest)] @=? output
  where
    str = "PRIVMSG #dikufags :this is a message\r\n"
    expected = BT.PRIVMSG
    rest = " #dikufags :this is a message\r\n"
    output = readP_to_S parseCommand str

parseCommand2 :: Assertion
parseCommand2 = [(expected, rest)] @=? output
  where
    str = "NOTICE #dikufags :this is a message\r\n"
    expected = BT.NOTICE
    rest = " #dikufags :this is a message\r\n"
    output = readP_to_S parseCommand str

parseCommand3 :: Assertion
parseCommand3 = [] @=? output
  where
    str = "NOTCOMMAND #dikufags :this is a message\r\n"
    output = readP_to_S parseCommand str

parseCommand4 :: Assertion
parseCommand4 = [(expected, rest)] @=? output
  where
    str = "004 #dikufags :this is a message\r\n"
    expected = BT.NUMCOM 4
    rest = " #dikufags :this is a message\r\n"
    output = readP_to_S parseCommand str

parseCommand5 :: Assertion
parseCommand5 = [] @=? output
  where
    str = "04 #dikufags :this is a message\r\n"
    output = readP_to_S parseCommand str

parseMessage1 :: Assertion
parseMessage1 = [(expected, "")] @=? nub output
  where
    str = ":bus000_!~fluttersh@1.1.1.1 PRIVMSG #dikufags_test :my message\r\n"
    expected = BT.IRCMessage prefix command params trailing
    prefix = Just $ BT.NicknamePrefix
        (BT.IRCUser "bus000_" (Just "~fluttersh") (Just "1.1.1.1"))
    command = BT.PRIVMSG
    params = ["#dikufags_test"]
    trailing = Just "my message"
    output = readP_to_S parseMessage str

parseMessage2 :: Assertion
parseMessage2 = [(expected, "")] @=? nub output
  where
    str = "PING :wolfe.freenode.net\r\n"
    expected = BT.IRCMessage prefix command params trailing
    prefix = Nothing
    command = BT.PING
    params = []
    trailing = Just "wolfe.freenode.net"
    output = readP_to_S parseMessage str

parseMessage3 :: Assertion
parseMessage3 = [(expected, "")] @=? nub output
  where
    str = ":wolfe.freenode.net NOTICE * :*** Looking up your hostname...\r\n"
    expected = BT.IRCMessage prefix command params trailing
    prefix = Just $ BT.ServernamePrefix "wolfe.freenode.net"
    command = BT.NOTICE
    params = ["*"]
    trailing = Just "*** Looking up your hostname..."
    output = readP_to_S parseMessage str

parseMessage4 :: Assertion
parseMessage4 = [(expected, "")] @=? nub output
  where
    str = ":wolfe.freenode.net 252 dikunttest123 28 :IRC Operators online\r\n"
    expected = BT.IRCMessage prefix command params trailing
    prefix = Just $ BT.ServernamePrefix "wolfe.freenode.net"
    command = BT.NUMCOM 252
    params = ["dikunttest123", "28"]
    trailing = Just "IRC Operators online"
    output = readP_to_S parseMessage str

parseMessage5 :: Assertion
parseMessage5 = [] @=? nub output
  where
    str = ":wolfe.freenode.net 252 dikunttest123 28 :IRC Operators online"
    output = readP_to_S parseMessage str

parseMessage6 :: Assertion
parseMessage6 = [(expected, "")] @=? nub output
  where
    str = ":_Hephaestus!~Ghost@152.115.87.26 PRIVMSG #dikufags :Hm\r\n"
    expected = BT.IRCMessage prefix command params trailing
    prefix = Just $ BT.nicknamePrefix "_Hephaestus" (Just "~Ghost")
        (Just "152.115.87.26")
    command = BT.PRIVMSG
    params = ["#dikufags"]
    trailing = Just "Hm"
    output = readP_to_S parseMessage str

{-":NickServ!NickServ@services. NOTICE dikunttest123 :dikunttest123 is nota registered nickname."-}
