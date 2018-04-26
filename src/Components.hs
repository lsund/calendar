
module Components where

import           Control.Monad       (forM_)
-- import           Data.Text           (append)
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Lucid
import           Protolude

import           CalendarDay


isPast :: TimeOfDay -> TimeOfDay -> Bool
isPast (TimeOfDay h m _) (TimeOfDay h' m' _)
    | h' < h            = True
    | h' == h && m' < m = True
    | otherwise         = False

-------------------------------------------------------------------------------
-- Classes

entryClasses :: Entry -> TimeOfDay -> [Attribute]
entryClasses e _
    | _done e             = [class_ "done mui--divider-bottom entry"]
    -- | ct `isPast` _time e = [class_ "past"]
    | otherwise           = [class_ "mui-form--inline entry"]


textClasses :: [Attribute]
textClasses = [class_ "mui-textfield"]

buttonClasses :: [Attribute]
buttonClasses = [class_ "mui-btn mui-btn--small"]


-------------------------------------------------------------------------------
-- Public API

entry :: Day -> Entry -> HtmlT Identity ()
entry d e
    | _done e = div_ (toHtml (show e :: Text))
    | otherwise = do
        form_ [method_ "post", action_ "update"] $
            div_ $ do
                input_ [type_ "hidden", name_ "id", value_ (show (_id e))]
                input_ [type_ "hidden", name_ "day", value_ (show d)]
                input_ [type_ "hidden", name_ "done", value_ (show (_done e))]
                input_ [type_ "text", name_ "desc", value_ (_desc e :: Text)]
                input_ [type_ "text", name_ "time", value_ (show $ _time e)]
                input_ [type_ "submit", value_ "update"]


        form_ [method_ "post", action_ "done"] $ do
            input_ [type_ "hidden", name_ "id", value_ (show (_id e))]
            input_ [type_ "submit", value_ "done"]


newEntry :: Int -> HtmlT Identity ()
newEntry id = do
    h2_ "New Entry"
    form_ [method_ "post", action_ "add"] $ do
        input_ [type_ "hidden", name_ "id", value_ (show id)]
        label_ $ do
            "Hour:"
            input_ [type_ "number", name_ "hour"]
        label_ $ do
            "Minute:"
            input_ [type_ "number", name_ "minute"]
        label_ $ do
            "Description: "
            textarea_ [name_ "desc"] ""
        br_ []
        input_
            [type_ "submit", value_ "Add Entry"]


day :: CalendarDay -> TimeOfDay -> HtmlT Identity ()
day (CalendarDay id d es) ct =
    div_ [class_ "day"] $ do
        h2_ $ toHtml (show d :: Text)
        ul_ $ forM_ (sortEntries es) (\e -> li_ (entryClasses e ct) $ entry d e)
        newEntry id
