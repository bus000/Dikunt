module Types.Internal.IRCUserTest (tests) where

import Data.Aeson (encode, decode)
import Test.Tasty (TestTree , testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types.Internal.IRCUser (IRCUser)

tests :: TestTree
tests = testGroup "Types.Internal.IRCUserTest"
    [ testProperty "jsonIdent" jsonIdent
    ]

jsonIdent :: IRCUser -> Bool
jsonIdent user = Just user == (decode . encode) user
