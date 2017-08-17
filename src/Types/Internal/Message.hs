{- |
 - Module      : Types.Internal.Message
 - Description : Representation of an IRC messages.
 - Copyright   : (c) Magnus Stavngaard, 2017
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - An IRC message is any non empty string consisting of anything but the
 - characters '\0', '\r' and '\n'.
 -}
module Types.Internal.Message where

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (suchThat)
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (isJust)
import qualified Data.Text as T

{- | The IRC message type. -}
newtype Message = Message String deriving (Show, Read, Eq)

{- | Smart constructor for Message's. -}
message :: String
    -- ^ Source of Message.
    -> Maybe Message
message [] = Nothing
message msg
    | not (any (`elem` "\0\r\n") msg) = Just $ Message msg
    | otherwise = Nothing

{- | Get the actual message. -}
getMessage :: Message
    -- ^ Message to get message from.
    -> String
getMessage (Message msg) = msg

{- | Construct arbitrary IRC Message's. -}
instance Arbitrary Message where
    arbitrary = Message <$> suchThat arbitrary (isJust . message)

    -- TODO: shrink.

{- | Convert Message's to JSON. -}
instance ToJSON Message where
    toJSON (Message msg) = Aeson.String . T.pack $ msg

{- | Parse Message's from JSON. -}
instance FromJSON Message where
    parseJSON = withText "message" $ return . Message . T.unpack

