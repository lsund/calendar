module Calendar.ViewComponents where

import           Control.Monad       (forM_)
import           Data.Text           as T
import           Data.Time.LocalTime
import           Lucid
import           Protolude

import qualified Calendar.CssClasses as C
import           Calendar.Data.Day
import qualified Calendar.Data.Todo as TODO
import           Calendar.Data.Entry
import           Calendar.Forecast (Weather)
import qualified Calendar.Forecast as FC

type TODO = TODO.TODO


showTime :: Maybe TimeOfDay -> Text
showTime (Just t) = T.take 5 $ show t
showTime Nothing = ""


isPast :: TimeOfDay -> TimeOfDay -> Bool
isPast (TimeOfDay h m _) (TimeOfDay h' m' _)
    | h' < h            = True
    | h' == h && m' < m = True
    | otherwise         = False


hiddenUpdateData :: Entry -> Date -> HtmlT Identity ()
hiddenUpdateData e d = do
    input_ [type_ "hidden", name_ "id", value_ (show (_id e))]
    input_ [type_ "hidden", name_ "day", value_ (show d)]
    input_ [type_ "hidden", name_ "done", value_ (show (_done e))]
    input_ [type_ "hidden", name_ "desc", value_ (_desc e :: Text)]


-------------------------------------------------------------------------------
-- Public API


entry :: Date -> Entry -> HtmlT Identity ()
entry d e
    | _done e = do
        let xd = if isNothing (_time e) then "sep-x-75" else "sep-x-20"
        span_ [class_ "time"] $ (toHtml . showTime . _time) e
        span_ [class_ xd] (toHtml ("" :: Text))
        span_ (toHtml (_desc e :: Text))
    | otherwise = do
        -- Update time
        span_ $
            form_ [class_ C.form, method_ "post", action_ "update"] $ do
                hiddenUpdateData e d
                div_ [class_ C.time] $
                    input_ [ type_ "text"
                           , name_ "time"
                           , value_ $ (showTime . _time) e]
        -- Update description
        span_ $
            form_ [class_ C.form, method_ "post", action_ "update"] $ do
                hiddenUpdateData e d
                input_ [ type_ "hidden"
                       , name_ "time"
                       , value_ (stripJust (show (_time e)))]
                div_ [class_ C.desc] $
                    input_ [ type_ "text"
                            , name_ "desc"
                            , value_ (_desc e :: Text)]
        -- Mark as done
        span_ $
            form_ [class_ C.form, method_ "post", action_ "done"] $ do
                input_ [type_ "hidden", name_ "id", value_ (show (_id e))]
                input_ [class_ C.button, type_ "submit", value_ "done"]
        -- Delete
        span_ $
            form_ [class_ C.form, method_ "post", action_ "delete"] $ do
                input_ [type_ "hidden", name_ "id", value_ (show (_id e))]
                input_ [class_ C.button, type_ "submit", value_ "del"]
        -- Push to next day
        span_ $
            form_ [class_ C.form, method_ "post", action_ "push"] $ do
                input_ [type_ "hidden", name_ "id", value_ (show (_id e))]
                input_ [class_ C.button, type_ "submit", value_ "push"]
    where
        stripJust = T.take 5 . T.drop 5


newEntry :: Int -> HtmlT Identity ()
newEntry id =
    form_ [class_ C.form, method_ "post", action_ "add"] $ do
        input_ [type_ "hidden", name_ "id", value_ (show id)]
        -- div_ [class_ C.time] $
        --     input_ [type_ "text", name_ "time", value_ "00:00"]
        div_ [class_ C.desc] $
            input_ [type_ "text", name_ "desc", placeholder_ "Description"]
        input_
            [class_ C.button, type_ "submit", value_ "Add Entry"]


day :: Day -> Maybe Weather -> TimeOfDay -> HtmlT Identity ()
day (Day id d es) wd ct =
    div_ [class_ "day"] $ do
        div_ [class_ "weather"] $ toHtml $ FC.weatherFormat wd
        div_ [class_ "date"] $ h2_ $ toHtml (dayFormat d)
        div_ [class_ "new"] $ newEntry id
        div_ [class_ "sep-y mui-divider"] ""
        div_ [class_ "entries"] $
            ul_ $ forM_ (sortEntries es)
                        (\e -> li_ [class_ $ C.entry e ct] $ entry d e)


todo :: [TODO] -> HtmlT Identity ()
todo es =
    div_ [class_ "todo"] $ do
        table_ $ do
            thead_ $
                tr_ $ do
                    th_ [class_ "first-todo-column"] "TodoItem"
                    th_ "Remove"
            tbody_ $
                forM_ es $ \e ->
                    tr_ $ do
                        td_ $ toHtml  (TODO._desc e)
                        td_ $
                            form_ [method_ "post", action_ "remove-todo"] $ do
                                input_ [ type_ "hidden"
                                       , name_ "id"
                                       , value_ (show (TODO._id e))]
                                input_ [type_ "submit", value_ "x"]
        div_ [class_ "todo-footer"] $
            form_ [class_ C.form, method_ "post", action_ "add-todo"] $ do
                input_ [type_ "text", name_ "desc", placeholder_ "Description"]
                input_ [class_ C.button, type_ "submit", value_ "Add"]


navbar :: Int -> HtmlT Identity ()
navbar id =
    div_ [class_ "navbar"] $ do
        span_ $
            form_ [method_ "get", action_ "day"] $ do
                input_ [type_ "hidden", name_ "id", value_ (show (pred id))]
                input_ [ class_ C.button
                        , type_ "submit"
                        , name_ "prev"
                        , value_ "Previous"]
        span_ [class_ "next"] $
            form_ [method_ "get", action_ "day"] $ do
                input_ [type_ "hidden", name_ "id", value_ (show (succ id))]
                input_ [ class_ C.button
                        , type_ "submit"
                        , name_ "action"
                        , value_ "Next"]
