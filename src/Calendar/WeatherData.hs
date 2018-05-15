module Calendar.WeatherData where

import           Calendar.Util
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

data WeatherData =
    WeatherData { _temp :: Double
                , _desc :: Text
                , _time :: LocalTime
                } deriving (Show)

apiEntry :: String
apiEntry = "http://api.openweathermap.org"

url :: String
url = "/data/2.5/forecast?q=Dusseldorf,de&APPID=0225725c608003e41c3e7936f6e6700b"

queryString :: String
queryString = apiEntry ++ url

toWeatherData :: Maybe (Value, Value, Value) -> Maybe WeatherData
toWeatherData (Just (Number x, String desc, String timeString)) =
    let temp = toRealFloat x
        time = parseTimeOrError True defaultTimeLocale "%F %X" (unpack timeString)
    in Just (WeatherData temp desc time)
toWeatherData _ = Nothing

readJSON :: String -> Maybe (HashMap Text Value)
readJSON = decode . encodeUtf8 . pack

toObject :: Value -> Object
toObject (Object o) = o
toObject _          = M.empty

toVector :: Value -> Vector Value
toVector (Array v) = v
toVector _         = V.empty

getTemp :: Object -> Maybe Value
getTemp o = M.lookup "main" o >>= M.lookup "temp" . toObject

getDesc :: Object -> Maybe Value
getDesc o =
    let weather = (! 0) . toVector <$> M.lookup "weather" o
    in weather >>= M.lookup "main" . toObject

getTime :: Object -> Maybe Value
getTime = M.lookup "dt_txt"


closestToNoon :: [Maybe WeatherData] -> Maybe WeatherData
closestToNoon =
    fromMaybe Nothing . find compMidday
    where
        compMidday (Just x) = (localTimeOfDay . _time) x >= midday
        compMidday Nothing  = False


getForecast :: IO [Maybe WeatherData]
getForecast = do
        (_, resp) <- liftIO $ curlGetString queryString []
        let (Just (Array days)) = readJSON resp >>= M.lookup "list"
            objs    = map toObject days
            triples = map sequence3 $ apply3 getTemp getDesc getTime (V.toList objs)
            datas   = map toWeatherData triples
            groups =  groupBy sameDay datas
        return $ map closestToNoon groups
        where
            sameDay (Just (WeatherData _ _ d1)) (Just (WeatherData _ _ d2)) =
                localDay d1 == localDay d2
            sameDay _ _ = False
