module Calendar.WeatherData where

import           Calendar.Util
import           Data.Aeson
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as M
import           Data.Text.Lazy          (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Vector             (Vector (..), (!))
import qualified Data.Vector             as V
import           Network.Curl
import           Prelude                 (String)
import           Protolude               hiding (encodeUtf8)

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
    let weather = (! 0) <$> toVector <$> M.lookup "weather" o
    in weather >>= M.lookup "main" . toObject

getTime :: Object -> Maybe Value
getTime = M.lookup "dt_txt"

getForecast :: IO [(Maybe Value, Maybe Value, Maybe Value)]
getForecast = do
        (_, resp) <- liftIO $ curlGetString "http://api.openweathermap.org/data/2.5/forecast?q=Dusseldorf,de&APPID=0225725c608003e41c3e7936f6e6700b" []
        let (Just (Array days)) = readJSON resp >>= M.lookup "list"
            objs = map toObject days
            -- temp =  x - 273.15
        return $ apply3 getTemp getDesc getTime (V.toList objs)
