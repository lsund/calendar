module Time where

import           Data.Time.LocalTime
import           GHC.Show            (Show, show)
import           Protolude


data Time = Time
    { _hour   :: Int
    , _minute :: Int
    }


instance Show Time where
    show (Time h m) = showi h <> ":" <> showi m
        where showi i
                | i < 10    = "0" <> GHC.Show.show i
                | otherwise = GHC.Show.show i


toTime :: LocalTime -> Time
toTime (LocalTime _ tod) = Time (todHour tod) (todMin tod)

isPast :: Time -> Time -> Bool
isPast (Time h m) (Time h' m')
    | h' < h            = True
    | h' == h && m' < m = True
    | otherwise         = False


