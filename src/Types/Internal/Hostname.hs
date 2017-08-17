{- |
 - Module      : Types.Internal.Hostname
 - Description : Representation of an IRC hostname.
 - Copyright   : (c) Magnus Stavngaard, 2017
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - An IRC hostname is alphanumeric strings separated by dots.
 -}
module Types.Internal.Hostname where

import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import qualified Data.Aeson.Types as Aeson
import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Parsers.Utils as PU
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (suchThat, listOf1, elements)
import qualified Text.Parsec as P

{- | IRC hostname. -}
newtype Hostname = Hostname String deriving (Show, Read, Eq)

{- | Smart constructor for Hostname's. -}
hostname :: String
    -- ^ Source of Hostname.
    -> Maybe Hostname
hostname host = case P.parse (PU.hostname <* P.eof) "(hostname source)" host of
    Right h -> return $ Hostname h
    Left _ -> Nothing

{- | Get the actual hostname. -}
getHostname :: Hostname
    -- ^ Hostname to get hostname from.
    -> String
getHostname (Hostname host) = host

{- | Construct arbitrary IRC Hostname's. -}
instance Arbitrary Hostname where
    arbitrary = Hostname <$> suchThat arbitraryHostname (isJust . hostname)
      where
        arbitraryHostname = intercalate "." <$> shortnames
        shortnames = listOf1 (listOf1 $ elements
            (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

    -- TODO: shrink.

{- | Convert Hostname's to JSON. -}
instance ToJSON Hostname where
    toJSON (Hostname host) = Aeson.String . T.pack $ host

{- | Parse Hostname's from JSON. -}
instance FromJSON Hostname where
    parseJSON = withText "user" $ return . Hostname . T.unpack
