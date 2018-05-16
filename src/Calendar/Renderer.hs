module Calendar.Renderer where

import           Data.Time.LocalTime
import           Lucid
import           Prelude                 (String)
import           Protolude
import           Web.Spock
import           Web.Spock.Lucid         (lucid)

import           Calendar.Data.Day
import qualified Calendar.ViewComponents as VC
import           Calendar.Forecast

-- pairForecast :: [Day] -> [WeatherData] -> [(Day, Maybe WeatherData)]
-- pairForecast days fc =

index :: (Show a, Num a, MonadIO m)
          => a
          -> TimeOfDay
          -> [Day]
          -> [Weather]
          -> ActionCtxT cxt m b
index temp t days fc =
    lucid $
        div_ [class_ "mui-container"] $ do
            link_ [rel_ "stylesheet", href_ "styles.css"]
            link_ [rel_ "stylesheet", href_ "mui.css"]

            h1_ $ toHtml $ timeFormat t <> ", Temp: " <> (show temp :: String)
            forM_ days $ \day -> VC.day day t
