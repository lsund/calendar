
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
    | otherwise =
        form_ [method_ "post", action_ "done"] $ do

            input_ [type_ "hidden", name_ "time", value_ (show $ _time e)]
            input_ [type_ "hidden", name_ "day", value_ (show d)]
            input_ $ buttonClasses <> [type_ "submit", value_ "done"]

            div_ textClasses $
                input_ [type_ "text", value_ (show e :: Text)]


day :: CalendarDay -> TimeOfDay -> HtmlT Identity ()
day (CalendarDay d es) ct =
    div_ [class_ "day"] $ do
        h2_ $ toHtml (show d :: Text)
        ul_ $ forM_ (sortEntries es) (\e -> li_ (entryClasses e ct) $ entry d e)


newEntry :: HtmlT Identity ()
newEntry = do
    h2_ "New Entry"
    form_ [method_ "post"] $ do
        label_ $ do
            "Hour:"
            input_ [type_ "number", name_ "hour"]
        label_ $ do
            "Minute:"
            input_ [type_ "number", name_ "minute"]
        br_ []
        label_ $ do
            "Description: "
            textarea_ [name_ "desc"] ""
        br_ []
        input_
            [type_ "submit", value_ "Add Entry"]
