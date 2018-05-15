module Calendar.Renderer where

import           Protolude
import           Data.Time.LocalTime
import           Web.Spock
import           Web.Spock.Lucid            (lucid)
import           Lucid
import Prelude (String)

import           Calendar.Data.Day
import qualified Calendar.ViewComponents    as VC

-- type Temperature = Double

index :: (Show a, Num a, MonadIO m)
          => a
          -> TimeOfDay
          -> [Day]
          -> ActionCtxT cxt m b
index temp t days =
    lucid $
        div_ [class_ "mui-container"] $ do
            link_ [rel_ "stylesheet", href_ "styles.css"]
            link_ [rel_ "stylesheet", href_ "mui.css"]

            h1_ $ toHtml $ timeFormat t <> ", Temp: " <> (show temp :: String)
            forM_ days $ \day -> VC.day day t
