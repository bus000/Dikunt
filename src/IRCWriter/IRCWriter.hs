{- |
 - Module      : IRCWriter.IRCWriter
 - Description : Write IRC messages defined in BotTypes module as strings.
 - Copyright   : (c) Magnus Stavngaard, 2016
 - License     : BSD-3
 - Maintainer  : magnus@stavngaard.dk
 - Stability   : experimental
 - Portability : POSIX
 -
 - Exports functions that can write IRC messages as strings ready to be send to
 - IRC channels. The writing of server messages is mostly for testing as the bot
 - will probably never run as a server.
 -}
module IRCWriter.IRCWriter
    ( writeMessage
    , writeServerMessage
    ) where

import IRCWriter.Impl
