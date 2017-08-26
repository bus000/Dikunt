module Types.Internal.ServerMessageTest (tests) where

import Data.Aeson (encode, decode)
import Test.Tasty (TestTree , testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types.Internal.ServerMessage (ServerMessage)
import Parsers.IRCMessageParser (parseMessage)
import IRCWriter.Impl (writeServerMessage)
import qualified Data.Text.Lazy as T

tests :: TestTree
tests = testGroup "Types.Internal.NicknameTest"
    [ testProperty "jsonIdent" jsonIdent
    , testProperty "parserIdent" parserIdent
    ]

jsonIdent :: ServerMessage -> Bool
jsonIdent serv = Just serv == (decode . encode) serv

parserIdent :: ServerMessage -> Bool
parserIdent serv = Right serv == (parseMessage . T.pack . writeServerMessage) serv
