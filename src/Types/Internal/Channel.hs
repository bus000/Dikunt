{- |
 - Module      : Types.Internal.Channel
 - Description : Representation of an IRC channel name.
 - Copyright   : (c) Magnus Stavngaard, 2017
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - An IRC channel starts with one of the characters #, + or & and is after that
 - any octet stream without the charaters '\0', '\a', '\r', '\n', ' ', ',' or
 - ':'.
 -}
module Types.Internal.Channel where

import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Parsers.Utils as PU
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)
import Test.QuickCheck.Gen (suchThat)
import qualified Text.Parsec as P

{- | IRC channel. -}
newtype Channel = Channel String deriving (Show, Read, Eq)

{- | Smart constructor for Channels, only allow correct IRC channels to be
 - constructed. -}
channel :: String
    -- ^ Source text of Channel.
    -> Maybe Channel
channel chan = case P.parse (PU.channel <* P.eof) "(channel source)" chan of
    Right c -> return $ Channel c
    Left _ -> Nothing

{- | Get the actual channel from the Channel type. -}
getChannel :: Channel
    -- ^ The Channel to get channel from.
    -> String
getChannel (Channel chan) = chan

{- | Construct arbitrary IRC channels. -}
instance Arbitrary Channel where
    arbitrary = Channel <$> suchThat arbitrary (isJust . channel)

    shrink (Channel chan) =
        [Channel chan' | chan' <- shrink chan
        , (isJust . channel) chan'
        ]

{- | Convert Channel's to JSON. -}
instance ToJSON Channel where
    toJSON (Channel chan) = Aeson.String . T.pack $ chan

{- | Parse Channel's from JSON. -}
instance FromJSON Channel where
    parseJSON = withText "channel" $ return . Channel . T.unpack

