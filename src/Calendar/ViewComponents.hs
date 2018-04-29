module Calendar.ViewComponents where

import           Control.Monad       (forM_)
import           Data.Text           as T
import           Data.Time.LocalTime
import           Lucid
import           Protolude

import qualified Calendar.CssClasses as C
import           Calendar.Data.Day
import           Calendar.Data.Entry


isPast :: TimeOfDay -> TimeOfDay -> Bool
isPast (TimeOfDay h m _) (TimeOfDay h' m' _)
    | h' < h            = True
    | h' == h && m' < m = True
    | otherwise         = False


-------------------------------------------------------------------------------
-- Public API

entry :: Date -> Entry -> HtmlT Identity ()
entry d e
    | _done e = div_ (toHtml (show e :: Text))
    | otherwise = do
        span_ $
            form_ [class_ C.form, method_ "post", action_ "update"] $ do
                input_ [type_ "hidden", name_ "id", value_ (show (_id e))]
                input_ [type_ "hidden", name_ "day", value_ (show d)]
                input_ [type_ "hidden", name_ "done", value_ (show (_done e))]
                div_ [class_ C.time] $
                    input_ [type_ "text", name_ "time", value_ $ T.take 5 (show $ _time e)]
                div_ [class_ C.desc] $
                    input_ [type_ "text", name_ "desc", value_ (_desc e :: Text)]
                input_ [class_ C.button, type_ "submit", value_ "update"]

        span_ $
            form_ [class_ C.form, method_ "post", action_ "done"] $ do
                input_ [type_ "hidden", name_ "id", value_ (show (_id e))]
                input_ [class_ C.button, type_ "submit", value_ "done"]

        span_ $
            form_ [class_ C.form, method_ "post", action_ "delete"] $ do
                input_ [type_ "hidden", name_ "id", value_ (show (_id e))]
                input_ [class_ C.button, type_ "submit", value_ "del"]


newEntry :: Int -> HtmlT Identity ()
newEntry id =
    form_ [class_ C.form, method_ "post", action_ "add"] $ do
        input_ [type_ "hidden", name_ "id", value_ (show id)]
        div_ [class_ C.time] $
            input_ [type_ "text", name_ "time", placeholder_ "hh:mm"]
        div_ [class_ C.desc] $
            input_ [type_ "text", name_ "desc", placeholder_ "Description"]
        input_
            [class_ C.button, type_ "submit", value_ "Add Entry"]


day :: Day -> TimeOfDay -> HtmlT Identity ()
day (Day id d es) ct =
    div_ [class_ "day"] $ do
        div_ [class_ "date"] $ h2_ $ toHtml (dayFormat d)
        div_ [class_ "new"] $ newEntry id
        div_ [class_ "sep mui-divider"] ""
        div_ [class_ "entries"] $
            ul_ $ forM_ (sortEntries es) (\e -> li_ [class_ $ C.entry e ct] $ entry d e)
