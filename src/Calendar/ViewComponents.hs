module Calendar.ViewComponents where

import           Control.Monad       (forM_)
import           Data.List           ((!!))
import qualified Data.Text           as T
import           Data.Time.LocalTime
import           Lucid
import           Protolude

import qualified Calendar.CssClasses as C
import           Calendar.Data.Day   (Day (..))
import qualified Calendar.Data.Day   as Day
import qualified Calendar.Data.Entry as Entry
import qualified Calendar.Data.Todo  as TODO
import           Calendar.Forecast   (Weather)

type TODO = TODO.TODO
type Date = Day.Date
type DayID = Int
type Entry = Entry.Entry
type Route = Text

showTime :: Maybe TimeOfDay -> Text
showTime (Just t) = T.take 5 $ show t
showTime Nothing  = ""


isPast :: TimeOfDay -> TimeOfDay -> Bool
isPast (TimeOfDay h m _) (TimeOfDay h' m' _)
    | h' < h            = True
    | h' == h && m' < m = True
    | otherwise         = False


hiddenUpdateData :: DayID -> Entry -> Date -> HtmlT Identity ()
hiddenUpdateData id e d = do
    input_ [type_ "hidden", name_ "entryid", value_ (show (Entry._id e))]
    input_ [type_ "hidden", name_ "done", value_ (show (Entry._done e))]


updateForm :: DayID -> Entry -> Text -> HtmlT Identity ()
updateForm id e action =
    form_ [class_ C.form , method_ "post" , action_ action] $ do
        input_ [type_ "hidden" , name_ "dayid" , value_ (show id)]
        input_ [type_ "hidden" , name_ "entryid" , value_ (show (Entry._id e))]
        input_ [class_ C.button, type_ "submit", value_ "x"]


dayOfWeekString :: Int -> Text
dayOfWeekString 1 = "Monday"
dayOfWeekString 2 = "Tuesday"
dayOfWeekString 3 = "Wednesday"
dayOfWeekString 4 = "Thursday"
dayOfWeekString 5 = "Friday"
dayOfWeekString 6 = "Saturday"
dayOfWeekString 7 = "Sunday"

emptyHTML :: HtmlT Identity ()
emptyHTML  = toHtml ("" :: Text)

-------------------------------------------------------------------------------
-- Public API


weekDay :: (Int, Day) -> HtmlT Identity ()
weekDay (n, d) = do
    div_ [class_ "mui-panel"] $ do
        a_ [href_ (Day.dateURL (_date d))] $
            toHtml $ dayOfWeekString n <> " " <> show (_date d)
        if (not . null) (_entries d)
            then div_ [class_ "has-entry"] emptyHTML
            else emptyHTML


entry :: Route -> DayID -> Date -> Entry -> HtmlT Identity ()
entry route id d e
    | Entry._done e =
        tr_ [class_ "done"] $ do
            td_ [class_ "done"] $ (toHtml . showTime . Entry._time) e
            td_ [class_ "done"] (toHtml (Entry._desc e :: Text))
            td_ [class_ "done"] emptyHTML
            td_ [class_ "done"] emptyHTML
            td_ [class_ "done"] emptyHTML

    | otherwise =
        tr_ $ do
            -- Update Entry
            td_ $
                form_ [class_ C.form
                      , method_ "post"
                      , action_ (route <> "/entry-update")] $ do
                    hiddenUpdateData id e d
                    input_ [type_ "hidden"
                           , name_ "desc"
                           , value_ (Entry._desc e :: Text)]
                    input_ [class_ C.time
                           , type_ "text"
                            , name_ "time"
                            , value_ $ (showTime . Entry._time) e]
            td_ $
                form_ [class_ C.form
                      , method_ "post"
                      , action_ (route <> "/entry-update")] $ do
                    hiddenUpdateData id e d
                    input_ [ type_ "hidden"
                        , name_ "time"
                        , value_ (stripJust (show (Entry._time e)))]
                    input_ [class_ C.desc
                            , type_ "text"
                            , name_ "desc"
                            , value_ (Entry._desc e :: Text)]
            -- Mark as done
            td_ $ updateForm id e (route <> "/entry-done")
            -- Delete
            td_ $ updateForm id e (route <> "/entry-delete")
            -- Push to next day
            td_ $ updateForm id e (route <> "/entry-push")
        where
            stripJust = T.take 5 . T.drop 5


entries :: Day -> HtmlT Identity ()
entries d@(Day id date es) =
    div_ [class_ "entries"] $
        if (not . null) es then
            table_ $ do
                thead_$
                    tr_ $ do
                        th_ [class_ "entry-time"] "Time"
                        th_ [class_ "entry-desc"] "Desc"
                        th_ [class_ "entry-button"] "Done"
                        th_ [class_ "entry-button"] "Delete"
                        th_ [class_ "entry-button"] "Push"
                tbody_ $
                    ul_ $ forM_ (Entry.sort es)
                        (tr_ . entry (Day.dayURL d) id date)
        else ""


newEntry :: Route -> DayID -> HtmlT Identity ()
newEntry route id =
    form_ [class_ C.form, method_ "post"
          , action_ (route <> "/entry-add")] $ do
        input_ [type_ "hidden" , name_ "dayid", value_ (show id)]
        div_ $ do
            h3_ $ toHtml ("New entry" :: Text)
            input_ [class_ C.time
                    , type_ "text"
                    , name_ "time"
                    , placeholder_ "hh:mm"]
            input_ [class_ C.desc, type_ "text", name_ "desc", placeholder_ "Description"]
            input_ [ class_ $ C.button <> " add-button"
                   , type_ "submit", value_ "Add Entry"]

day :: Day -> Maybe Weather -> TimeOfDay -> HtmlT Identity ()
day d@(Day id date _) _ tod =
    div_ [class_ "day"] $ do
        div_ [class_ "time"] $ toHtml $ Day.timeFormat tod
        div_ [class_ "center-wrapper"] $ do
            h2_ [class_ "center"] $ toHtml $ Day.dateFormat date
            entries d
        div_ [class_ "new"] $ newEntry (Day.dayURL d) id


todo :: Day -> [TODO] -> HtmlT Identity ()
todo d todos =
    div_ [class_ "todo"] $ do
        table_ [class_ "todo-table"] $ do
            thead_ $
                tr_ $ do
                    th_ "TodoItem"
                    th_ "Remove"
            tbody_ $
                forM_ todos $ \e ->
                    tr_ $ do
                        td_ $
                            form_ [ method_ "post"
                                  , action_ (Day.dayURL d <>
                                             "/todo-update")] $ do
                                input_ [ type_ "hidden"
                                       , name_ "todoid"
                                       , value_ (show (TODO._id e))]
                                input_ [ class_ "todo-desc"
                                       , type_ "text"
                                       , name_ "desc"
                                       , value_ (TODO._desc e)]
                        td_ $
                            form_ [method_ "post"
                                  , action_ (Day.dayURL d <>
                                             "/todo-remove")] $ do
                                input_ [ type_ "hidden"
                                       , name_ "todoid"
                                       , value_ (show (TODO._id e))]
                                input_ [ type_ "submit"
                                       , value_ "x"]
        div_ [class_ "todo-footer"] $
            form_ [class_ C.form, method_ "post"
                  , action_ (Day.dayURL d <> "/todo-add")] $ do
                input_ [type_ "text", name_ "desc", placeholder_ "Description"]
                input_ [class_ $ C.button <> " add-button"
                       , type_ "submit"
                       , value_ "Add"]


navbar :: Date -> HtmlT Identity ()
navbar date =
    div_ [class_ "mui-appbar"] $
        table_ [width_ "100%"] $
            tr_ [style_ "tvertical-align:middle;"] $ do
                td_  [class_ "mui--appbar-height", width_ "33%"] $
                    form_ [method_ "get", action_ (Day.dateURL date <> "/week")] $
                        input_ [ class_ C.button , type_ "submit", value_ "Week"]
                td_  [class_ "mui--appbar-height", width_ "33%"] $ do
                    form_ [method_ "get", action_ (Day.dateURL (pred date))] $
                        input_ [ class_ C.button , type_ "submit", value_ "Previous"]
                    form_ [method_ "get", action_ (Day.dateURL (succ date))] $
                        input_ [ class_ C.button , type_ "submit", value_ "Next"]
                td_  [class_ "mui--appbar-height", width_ "33%"] $
                    form_ [method_ "post", action_ "/browseDate"] $ do
                        input_ [ type_ "date", name_ "date"]
                        input_ [ type_ "submit", style_ "visibility: hidden;"]


weekNavbar :: Date -> HtmlT Identity ()
weekNavbar date =
    div_ [class_ "mui-appbar"] $
        table_ [width_ "100%"] $
            tr_ [style_ "tvertical-align:middle;"] $ do
                td_  [class_ "mui--appbar-height", width_ "33%"] $
                    form_ [method_ "get", action_ (Day.dateURL date <> "/week")] $
                        input_ [ class_ C.button , type_ "submit", value_ "Week"]
                td_  [class_ "mui--appbar-height", width_ "33%"] $ do
                    form_ [ method_ "get"
                          , action_ (Day.dateURL (enumFromThen date (pred date) !! 7) <> "/week") ] $
                        input_ [ class_ C.button , type_ "submit", value_ "Previous"]
                    form_ [ method_ "get"
                          , action_ (Day.dateURL (enumFrom date !! 7) <> "/week")] $
                        input_ [ class_ C.button , type_ "submit", value_ "Next"]
                td_  [class_ "mui--appbar-height", width_ "33%"] $
                    form_ [method_ "post", action_ "/browseDate"] $ do
                        input_ [ type_ "date", name_ "date"]
                        input_ [ type_ "submit", style_ "visibility: hidden;"]
