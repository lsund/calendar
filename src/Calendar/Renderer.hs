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
        f day@(Day _ date _) =
            case find (\w -> localDay (_time w) == date) fc of
                    Just w  -> (day, Just w)
                    Nothing -> (day, Nothing)

index :: MonadIO m => TimeOfDay -> [Day] -> [Weather] -> [TODO] -> ActionCtxT cxt m b
index t days fc todos =
    lucid $
        div_ [class_ "content"] $ do
            div_ (VC.todo todos)
            div_ $ do
                link_ [rel_ "stylesheet", href_ "styles.css"]
                link_ [rel_ "stylesheet", href_ "mui.css"]
                title_ "Calendar"

                h1_ $ toHtml $ timeFormat t
                forM_ (pairForecast days fc) $
                    \(day, wd) -> VC.day day wd t
