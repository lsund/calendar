module Calendar.Forecast where

import           Data.Aeson
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as M
import           Data.List               (groupBy)
import           Data.Maybe
import           Data.Scientific
import           Data.Text               (unpack)
import           Data.Text.Lazy          (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Vector             (Vector, (!))
import qualified Data.Vector             as V
import           Network.Curl
import           Prelude                 (String)
import           Protolude               hiding (encodeUtf8)

import           Calendar.Data.Day
import           Calendar.Util

data Weather = Weather
        { _temp :: Double
        , _desc :: Text
        , _time :: LocalTime
        } deriving (Show)


apiEntry :: String
apiEntry = "http://api.openweathermap.org"


url :: String
url = "/data/2.5/forecast?q=Dusseldorf,de&APPID=0225725c608003e41c3e7936f6e6700b"


queryString :: String
queryString = apiEntry ++ url


zeroKelvin :: Double
zeroKelvin = 273.15

weatherFormat :: Maybe Weather -> Text
weatherFormat (Just (Weather temp desc (LocalTime _ t))) =
    "Weather at " <> timeFormat t <> ": " <> show (round temp) <> " degrees " <> desc
weatherFormat Nothing = "No data"


toWeather :: Maybe (Value, Value, Value) -> Maybe Weather
toWeather (Just (Number x, String desc, String timeString)) =
    let temp = toRealFloat x - zeroKelvin
        time = parseTimeOrError True defaultTimeLocale "%F %X" (unpack timeString)
    in Just (Weather temp desc time)
toWeather _ = Nothing


readJSON :: String -> Maybe (HashMap Text Value)
readJSON = decode . encodeUtf8 . pack


toObject :: Value -> Object
toObject (Object o) = o
toObject _          = M.empty


toVector :: Value -> Vector Value
toVector (Array v) = v
toVector _         = V.empty


closestToMidday :: [Maybe Weather] -> Maybe Weather
closestToMidday []       = Nothing
closestToMidday (x : xs) = (fromMaybe x . find atLeastMidday) xs
    where
        atLeastMidday (Just x') = (localTimeOfDay . _time) x' >= midday
        atLeastMidday Nothing   = False


sameDay :: Maybe Weather -> Maybe Weather -> Bool
sameDay (Just d1) (Just d2) = (localDay . _time) d1 == (localDay . _time) d2
sameDay _ _                 = False


getValues :: [Object] -> [Maybe (Value, Value, Value)]
getValues = map sequence3 . apply3 getTemp getDesc getTime
    where
        getTemp o = M.lookup "main" o >>= M.lookup "temp" . toObject
        getTime = M.lookup "dt_txt"
        getDesc o =
            let weather = (! 0) . toVector <$> M.lookup "weather" o
            in weather >>= M.lookup "main" . toObject


getForecast :: IO (Maybe [Weather])
getForecast = do
        (_, resp) <- liftIO $ curlGetString queryString []

        let (Just (Array days)) = readJSON resp >>= M.lookup "list"
            values = getValues $ V.toList $ map toObject days
            datas  = groupBy sameDay $ map toWeather values

        return $ mapM closestToMidday datas
