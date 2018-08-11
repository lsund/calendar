module Calendar.Renderer where

import           Data.Time.LocalTime
import           Lucid
import           Protolude
import           Web.Spock
import           Web.Spock.Lucid         (lucid)

import           Calendar.Data.Day (Day(..))
import qualified Calendar.Data.Day       as Day
import qualified Calendar.Data.Todo      as TODO
import           Calendar.Forecast
import qualified Calendar.ViewComponents as VC

type TODO = TODO.TODO

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
            link_ [rel_ "stylesheet", href_ "/styles.css"]
            link_ [rel_ "stylesheet", href_ "/mui.css"]
            title_ "Calendar"
        body_ $ div_ [class_ "content"] b


index :: MonadIO m => TimeOfDay -> [Day] -> [Weather] -> [TODO] -> ActionCtxT cxt m b
index t days fc todos =
    layout $ do
        div_ (VC.todo 0 todos)
        h1_ $ toHtml $ Day.timeFormat t
        forM_ (pairForecast days fc) $
            \(d, wd) -> VC.day d wd t


day :: MonadIO m => Day -> TimeOfDay -> Maybe Weather -> [TODO] -> ActionCtxT cxt m b
day d tod w todos =
    layout $ do
        div_ (VC.todo (Day._id d) todos)
        div_ [class_ "center-wrapper"] $
            span_ [class_ "center"] $
                h1_ [class_ "time"] $ toHtml $ Day.timeFormat tod
        VC.navbar (Day._date d)
        VC.day d w tod

err :: MonadIO m => Text -> ActionCtxT cxt m b
err msg = layout $ do
            h1_ $ toHtml ("Error" :: Text)
            div_ $ toHtml msg

