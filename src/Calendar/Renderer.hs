module Calendar.Renderer where

import           Data.Time.LocalTime
import           Lucid
import           Protolude
import           Web.Spock
import           Web.Spock.Lucid         (lucid)

import           Calendar.Data.Day
import           Calendar.Data.Todo
import           Calendar.Forecast
import qualified Calendar.ViewComponents as VC

pairForecast :: [Day] -> [Weather] -> [(Day, Maybe Weather)]
pairForecast days fc = map f days
    where
        f d@(Day _ date _) =
            case find (\w -> localDay (_time w) == date) fc of
                    Just w  -> (d, Just w)
                    Nothing -> (d, Nothing)


layout :: MonadIO m => HtmlT Identity a -> ActionCtxT cxt m b
layout b =
    lucid $ do
        head_ $ do
            link_ [rel_ "stylesheet", href_ "styles.css"]
            link_ [rel_ "stylesheet", href_ "mui.css"]
            title_ "Calendar"
        body_ $ div_ [class_ "content"] b


index :: MonadIO m => TimeOfDay -> [Day] -> [Weather] -> [TODO] -> ActionCtxT cxt m b
index t days fc todos =
    layout $ do
        div_ (VC.todo todos)
        h1_ $ toHtml $ timeFormat t
        forM_ (pairForecast days fc) $
            \(d, wd) -> VC.day d wd t


day :: MonadIO m => TimeOfDay -> Day -> Maybe Weather -> [TODO] -> ActionCtxT cxt m b
day t d w todos =
    layout $ do
        div_ (VC.todo todos)
        div_ [class_ "center-wrapper"] $
            span_ [class_ "center"] $
                h1_ [class_ "time"] $ toHtml $ timeFormat t
        VC.navbar
        VC.day d w t
