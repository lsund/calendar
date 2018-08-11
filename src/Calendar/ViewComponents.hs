module Calendar.ViewComponents where

import           Control.Monad       (forM_)
import           Data.Text           as T
import           Data.Time.LocalTime
import           Lucid
import           Protolude

import qualified Calendar.CssClasses as C
import           Calendar.Data.Day   (Day (..))
import qualified Calendar.Data.Day   as Day
import qualified Calendar.Data.Entry as Entry
import qualified Calendar.Data.Todo  as TODO
import           Calendar.Forecast   (Weather)
import qualified Calendar.Forecast   as FC

type TODO = TODO.TODO
type Date = Day.Date
type DayID = Int
type Entry = Entry.Entry

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
    input_ [type_ "hidden", name_ "dayid", value_ (show id)]
    input_ [type_ "hidden", name_ "entryid", value_ (show (Entry._id e))]
    input_ [type_ "hidden", name_ "day", value_ (show d)]
    input_ [type_ "hidden", name_ "done", value_ (show (Entry._done e))]


updateForm :: DayID -> Entry -> Text -> HtmlT Identity ()
updateForm id e action =
    form_ [class_ C.form , method_ "post" , action_ action] $ do
        input_ [type_ "hidden" , name_ "dayid" , value_ (show id)]
        input_ [type_ "hidden" , name_ "entryid" , value_ (show (Entry._id e))]
        input_ [class_ C.button, type_ "submit", value_ "x"]



-------------------------------------------------------------------------------
-- Public API


entry :: DayID -> Date -> Entry -> HtmlT Identity ()
entry id d e
    | Entry._done e =
        tr_ [class_ "done"] $ do
            td_ [class_ "done"] $ (toHtml . showTime . Entry._time) e
            td_ [class_ "done"] (toHtml (Entry._desc e :: Text))
            td_ [class_ "done"] (toHtml ("" :: Text))
            td_ [class_ "done"] (toHtml ("" :: Text))
            td_ [class_ "done"] (toHtml ("" :: Text))

    | otherwise =
        tr_ $ do
            td_ $
                form_ [class_ C.form
                      , method_ "post"
                      , action_ "update"] $ do
                    hiddenUpdateData id e d
                    input_ [type_ "hidden"
                           , name_ "desc"
                           , value_ (Entry._desc e :: Text)]
                    input_ [class_ C.time
                           , type_ "text"
                            , name_ "time"
                            , value_ $ (showTime . Entry._time) e]
            -- Update description
            td_ $
                form_ [class_ C.form
                      , method_ "post"
                      , action_ "update"] $ do
                    hiddenUpdateData id e d
                    input_ [ type_ "hidden"
                        , name_ "time"
                        , value_ (stripJust (show (Entry._time e)))]
                    input_ [class_ C.desc
                            , type_ "text"
                            , name_ "desc"
                            , value_ (Entry._desc e :: Text)]
            -- Mark as done
            td_ $ updateForm id e "done"
            -- Delete
            td_ $ updateForm id e "delete"
            -- Push to next day
            td_ $ updateForm id e "push"
        where
            stripJust = T.take 5 . T.drop 5


newEntry :: Int -> HtmlT Identity ()
newEntry id =
    form_ [class_ C.form, method_ "post", action_ "add"] $ do
        input_ [type_ "hidden", name_ "dayid", value_ (show id)]
        div_ [class_ C.desc] $ do
            input_ [type_ "text", name_ "desc", placeholder_ "Description"]
            input_
                [ class_ $ C.button <> " add-button"
                , type_ "submit", value_ "Add Entry"]


day :: Day -> Maybe Weather -> TimeOfDay -> HtmlT Identity ()
day (Day id d es) wd _ =
    div_ [class_ "day"] $ do
        div_ [class_ "weather"] $ toHtml $ FC.weatherFormat wd
        div_ [class_ "date"] $ h2_ $ toHtml (Day.dayFormat d)
        div_ [class_ "new"] $ newEntry id
        div_ [class_ "sep-y mui-divider"] ""
        div_ [class_ "entries"] $
            table_ $ do
                thead_$
                    tr_ $ do
                        th_ "Time"
                        th_ "Desc"
                        th_ "Done"
                        th_ "Delete"
                        th_ "Push"
                tbody_ $
                    ul_ $ forM_ (Entry.sort es)
                        (tr_ . entry id d)


todo :: DayID -> [TODO] -> HtmlT Identity ()
todo id todos =
    div_ [class_ "todo"] $ do
        table_ [class_ "todo-table"] $ do
            thead_ $
                tr_ $ do
                    th_ [class_ "first-todo-column"] "TodoItem"
                    th_ "Remove"
            tbody_ $
                forM_ todos $ \e ->
                    tr_ $ do
                        td_ $
                            form_ [class_ C.form
                                  , method_ "post"
                                  , action_ "update-todo"] $ do
                                input_ [type_ "hidden"
                                       , name_ "dayid", value_ (show id)]
                                input_ [ type_ "hidden"
                                       , name_ "todoid"
                                       , value_ (show (TODO._id e))]
                                input_ [ type_ "text"
                                       , name_ "desc"
                                       , value_ (TODO._desc e)]
                        td_ $
                            form_ [method_ "post"
                                  , action_ "remove-todo"] $ do
                                input_ [type_ "hidden"
                                       , name_ "dayid", value_ (show id)]
                                input_ [ type_ "hidden"
                                       , name_ "todoid"
                                       , value_ (show (TODO._id e))]
                                input_ [class_ C.button
                                       , type_ "submit"
                                       , value_ "x"]
        div_ [class_ "todo-footer"] $
            form_ [class_ C.form, method_ "post", action_ "add-todo"] $ do
                input_ [type_ "hidden", name_ "dayid", value_ (show id)]
                input_ [type_ "text", name_ "desc", placeholder_ "Description"]
                input_ [class_ $ C.button <> " add-button"
                       , type_ "submit"
                       , value_ "Add"]


navbar :: Date -> HtmlT Identity ()
navbar date =
    div_ [class_ "navbar"] $ do
        span_ $
            form_ [method_ "get", action_ (Day.dateURL (pred date))] $
                input_ [ class_ C.button , type_ "submit", value_ "previous"]
        span_ [class_ "next"] $
            form_ [method_ "get", action_ (Day.dateURL (succ date))] $
                input_ [ class_ C.button , type_ "submit", value_ "next"]
