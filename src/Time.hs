
module Time where

import GHC.Show
import Protolude hiding (Show, show)
import           Data.Time.LocalTime


data Time = Time
    { _hour   :: Int
    , _minute :: Int
    }


instance Show Time where
    show (Time h m) = show h ++ ":" ++ showi m
        where showi i
                | i < 10    = "0" ++ show i
                | otherwise = show i


toTime :: LocalTime -> Time
toTime (LocalTime d tod) = Time (todHour tod) (todMin tod)


isPast :: Time -> Time -> Bool
isPast (Time h m) (Time h' m')
    | h' < h            = True
    | h' == h && m' < m = True
    | otherwise         = False

