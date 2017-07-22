{- |
 - Module      : IRCParser.Parser
 - Description : Parses IRC messages.
 - Copyright   : (c) Magnus Stavngaard, 2016
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - Parse IRC messages from a string to an IRCMessage the general structure of an
 - IRC message is
 -
 -     message = [ ":" prefix SPACE ] command [ params ] crlf.
 -
 - See https://tools.ietf.org/html/rfc2812 for details.
 -}
module IRCParser.IRCMessageParser ( parseMessage ) where

import IRCParser.Layer1Impl ( parseIRCMessage )
import IRCParser.Layer2Impl ( parseSM )
import BotTypes ( ServerMessage )

parseMessage :: String -> Maybe ServerMessage
parseMessage str = parseIRCMessage str >>= parseSM
