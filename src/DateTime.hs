module DateTime where

import           Data.Time.Calendar
import           Data.Time.LocalTime


data DateTime = DateTime { _date :: Day, _time :: TimeOfDay }

toDateTime :: LocalTime -> DateTime
toDateTime lt = DateTime (localDay lt) (localTimeOfDay lt)
