{- |
 - Module      : Types.Internal.Servername
 - Description : Representation of an IRC server name.
 - Copyright   : (c) Magnus Stavngaard, 2017
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - An IRC servername is either an IPV4 address, an IPV6 address or a hostname
 - consisting of alphanumeric strings separated by dots.
 -}
module Types.Internal.Servername where

import Types.Internal.Hostname (getHostname)
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import qualified Data.Aeson.Types as Aeson
import qualified Parsers.Utils as PU
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneof)
import qualified Text.Parsec as P
import qualified Data.Text as T
import Data.Either (rights)
import Data.List (intercalate)
import Control.Monad (replicateM)

{- | IRC servername. -}
newtype Servername = Servername String deriving (Show, Read, Eq)

{- | Smart constructor for Servername's. -}
servername :: String
    -- ^ Source of servername.
    -> Maybe Servername
servername serv = case rights [ipV4Servername, ipV6Servername, hostServername] of
    (x:_) -> return $ Servername x
    _ -> Nothing
  where
    ipV4Servername = P.parse (PU.ipV4 <* P.eof) "(ipv4 source)" serv
    ipV6Servername = P.parse (PU.ipV6 <* P.eof) "(ipv6 source)" serv
    hostServername = P.parse (PU.hostname <* P.eof) "(hostname source)" serv

{- | Get the actual server name. -}
getServerName :: Servername
    -- ^ Servername to get servername from.
    -> String
getServerName (Servername server) = server

{- | Construct arbitrary IRC servernames. -}
instance Arbitrary Servername where
    arbitrary = Servername <$> oneof [arbitraryIPV6, arbitraryIPV4, arbitraryHost]
      where
        arbitraryIPV6 = intercalate ":" <$> replicateM 8 pos
        arbitraryIPV4 = intercalate "." <$> replicateM 4 pos
        arbitraryHost = getHostname <$> arbitrary
        pos = (show . abs :: Integer -> String) <$> arbitrary

{- | Convert Servername's to JSON. -}
instance ToJSON Servername where
    toJSON (Servername server) = Aeson.String . T.pack $ server

{- | Parse Servername's from JSON. -}
instance FromJSON Servername where
    parseJSON = withText "server" $ return . Servername . T.unpack
