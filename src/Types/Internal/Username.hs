{- |
 - Module      : Types.Internal.Username
 - Description : Representation of an IRC username.
 - Copyright   : (c) Magnus Stavngaard, 2017
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - An IRC username is a non-empty string of characters not including '\0', '\r',
 - '\n', ' ', '@', '%'.
 -}
module Types.Internal.Username where

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (suchThat)
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import qualified Data.Aeson.Types as Aeson
import qualified Parsers.Utils as PU
import qualified Text.Parsec as P
import Data.Maybe (isJust)
import qualified Data.Text as T

{- | IRC username. -}
newtype Username = Username String deriving (Show, Read, Eq)

{- | Smart constructor for Username's. -}
username :: String
    -- ^ Source of username.
    -> Maybe Username
username user = case P.parse (PU.username <* P.eof) "(username source)" user of
    Right u -> return $ Username u
    Left _ -> Nothing

{- | Get the actual username. -}
getUsername :: Username -> String
getUsername (Username user) = user

{- | Construct arbitrary IRC Username's. -}
instance Arbitrary Username where
    arbitrary = Username <$> suchThat arbitrary (isJust . username)

    -- TODO: shrink.

{- | Convert Username's to JSON. -}
instance ToJSON Username where
    toJSON (Username user) = Aeson.String . T.pack $ user

{- | Parse Username's from JSON. -}
instance FromJSON Username where
    parseJSON = withText "user" $ return . Username . T.unpack
