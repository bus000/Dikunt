module Types.Internal.HostnameTest (tests) where

import Data.Aeson (encode, decode)
import qualified Data.Char as Char
import Data.Maybe (isNothing, isJust)
import Test.Tasty (TestTree , testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types.Internal.Hostname (Hostname(..), getHostname, hostname)

tests :: TestTree
tests = testGroup "Hostname Tests"
    [ testProperty "jsonIdent" jsonIdent
    , testProperty "startsWith" startsWith
    , testProperty "minSize" minSize
    , testProperty "getHostnameIdent" getHostnameIdent
    , testProperty "dontContain" dontContain
    , testProperty "dontStartWithDot" dontStartWithDot
    , testProperty "dontEndWithDot" dontEndWithDot
    , testProperty "extend" extend
    ]

jsonIdent :: Hostname -> Bool
jsonIdent host = Just host == (decode . encode) host

startsWith :: Hostname -> Bool
startsWith host = Char.isAlphaNum $ (head . getHostname) host

minSize :: Hostname -> Bool
minSize host = (length . getHostname) host >= 1

getHostnameIdent :: Hostname -> Bool
getHostnameIdent host = Just host == (hostname . getHostname) host

dontContain :: Hostname -> Bool
dontContain host = not $ any (`elem` "\0\a\r\n ,:") (getHostname host)

dontStartWithDot :: Hostname -> Bool
dontStartWithDot host = isNothing $ hostname ('.':getHostname host)

dontEndWithDot :: Hostname -> Bool
dontEndWithDot host = isNothing $ hostname (getHostname host ++ ".")

extend :: Hostname -> Hostname -> Bool
extend (Hostname host1) (Hostname host2) = isJust $ hostname (host1 ++ "." ++ host2)
