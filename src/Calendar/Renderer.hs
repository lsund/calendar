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


day :: MonadIO m => Day -> TimeOfDay -> Maybe Weather -> [TODO] -> ActionCtxT cxt m b
day d tod w todos =
    layout $
        div_ [class_ "mui-container"] $ do
            VC.navbar (Day._date d)
            VC.day d w tod
            div_ (VC.todo d todos)


week :: MonadIO m => Int -> [Day.Date] -> ActionCtxT ctx m b
week wn ds =
    layout $
        div_ [class_ "mui-container"] $ do
            div_ [class_ "mui-appbar"]
                (h3_ $ toHtml ("Week " <> show wn :: Text))
            ul_ $ forM_ (zip [1..] ds) (li_ . VC.weekDay)



err :: MonadIO m => Text -> ActionCtxT cxt m b
err msg = layout $ do
            h1_ $ toHtml ("Error" :: Text)
            div_ $ toHtml msg

