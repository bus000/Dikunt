module Functions.News
    ( news
    ) where

import Control.Monad.State (liftIO)
import qualified BotTypes as BT
import Network.Download (openAsFeed)
import Text.Feed.Query
    ( feedItems
    , getItemTitle
    , getItemLink
    , getItemDescription
    )
import Text.Feed.Types (Item)

news :: BT.BotFunction
news = BT.BotFunction
    { BT.shouldRun = shouldRun
    , BT.run = run
    , BT.help = "<nick>: news - Give you the latest breaking news."
    , BT.name = "News"
    }

shouldRun :: BT.Message -> BT.Net Bool
shouldRun (BT.PrivMsg _ _ msg) = do
    nick <- BT.getValue BT.nickname
    case words msg of
        [first, "news"] -> return $ first == (nick ++ ":")
        _ -> return False
shouldRun _ = return False

{- TODO: refactor, this is FUGLY. -}
run :: BT.Message -> BT.Net [BT.Message]
run _ = do
    feed <- liftIO $ openAsFeed rssFeed
    case feed of
        Left err -> BT.privmsgs err
        Right corFeed -> case feedItems corFeed of
            (breaking:_) -> case analyseFeed breaking of
                Just str -> BT.privmsgs str
                Nothing -> BT.privmsgs "Feed analysation failed"
            _ -> BT.privmsgs "No news"

analyseFeed :: Item -> Maybe String
analyseFeed item = do
    title <- getItemTitle item
    link <- getItemLink item
    description <- getItemDescription item

    return $ title ++ "\n    " ++ description ++ "\n" ++ link

rssFeed :: String
rssFeed = "http://feeds.bbci.co.uk/news/world/rss.xml"
