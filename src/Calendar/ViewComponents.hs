module Calendar.ViewComponents where

import           Control.Monad       (forM_)
import           Data.Time.LocalTime
import           Lucid
import           Protolude

import           Calendar.Data.Day
import           Calendar.Data.Entry


isPast :: TimeOfDay -> TimeOfDay -> Bool
isPast (TimeOfDay h m _) (TimeOfDay h' m' _)
    | h' < h            = True
    | h' == h && m' < m = True
    | otherwise         = False

-------------------------------------------------------------------------------
-- Classes

entryClasses :: Entry -> TimeOfDay -> [Attribute]
entryClasses e _
    | _done e             = [class_ "done mui--divider-bottom"]
    -- | ct `isPast` _time e = [class_ "past"]
    | otherwise           = [class_ "mui-form--inline"]


textClasses :: [Attribute]
textClasses = [class_ "mui-textfield"]

buttonClasses :: [Attribute]
buttonClasses = [class_ "mui-btn mui-btn--small"]


-------------------------------------------------------------------------------
-- Public API

entry :: Date -> Entry -> HtmlT Identity ()
entry d e
    | _done e = div_ (toHtml (show e :: Text))
    | otherwise =
        div_  $ do
            div_ [class_ "left"] $
                form_ [method_ "post", action_ "update"] $
                    div_ $ do
                        input_ [type_ "hidden", name_ "id", value_ (show (_id e))]
                        input_ [type_ "hidden", name_ "day", value_ (show d)]
                        input_ [type_ "hidden", name_ "done", value_ (show (_done e))]
                        input_ [class_ "time", type_ "text", name_ "time", value_ (show $ _time e)]
                        input_ [class_ "desc", type_ "text", name_ "desc", value_ (_desc e :: Text)]
                        input_ [type_ "submit", value_ "update"]

            div_ $
                form_ [method_ "post", action_ "done"] $ do
                    input_ [type_ "hidden", name_ "id", value_ (show (_id e))]
                    input_ [type_ "submit", value_ "done"]


newEntry :: Int -> HtmlT Identity ()
newEntry id =
    form_ [method_ "post", action_ "add"] $ do
        input_ [type_ "hidden", name_ "id", value_ (show id)]
        input_ [class_ "time", type_ "text", name_ "time", value_ "00:00"]
        textarea_ [name_ "desc"] ""
        input_
            [type_ "submit", value_ "Add Entry"]


day :: Day -> TimeOfDay -> HtmlT Identity ()
day (Day id d es) ct =
    div_ [class_ "day"] $ do
        h2_ [class_ "date"] $ toHtml (dayFormat d)
        div_ [class_ "entries"] $
            ul_ $ forM_ (sortEntries es) (\e -> li_ (entryClasses e ct) $ entry d e)

        div_ [class_ "mui-divider"] ""

        div_  [class_ "newentry"] $
            newEntry id
