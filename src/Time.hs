module Time where

import           Data.Time.Calendar
import           Data.Time.LocalTime
import           GHC.Show            (Show, show)
import           Protolude           hiding (show)


data Date = Date
    { _year  :: Integer
    , _month :: Int
    , _day   :: Int
    } deriving (Show)


data Time = Time
    { _hour   :: Int
    , _minute :: Int
    }


instance Show Time where
    show (Time h m) = show h <> ":" <> showi m
        where showi i
                | i < 10    = "0" <> GHC.Show.show i
                | otherwise = GHC.Show.show i


toTime :: LocalTime -> Time
toTime (LocalTime _ tod) = Time (todHour tod) (todMin tod)

toDate :: LocalTime -> Date
toDate (LocalTime day _) = let (y, m, d) = toGregorian day in Date y m d


isPast :: Time -> Time -> Bool
isPast (Time h m) (Time h' m')
    | h' < h            = True
    | h' == h && m' < m = True
    | otherwise         = False


dateToPath :: Date -> FilePath
dateToPath (Date y m d) =
    concat ["data/", show y, "/", padd m, "/", padd d, ".txt"]
    where
        padd x
            | x < 10    = "0" ++ show x
            | otherwise = show x


-- Cannot implement enum for dates, since `_year` is Integer, not Int
succDate :: Date -> Date
succDate (Date y m d) = Date y m $ succ d

