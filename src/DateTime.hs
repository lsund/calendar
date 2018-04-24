module DateTime where

import           Data.Time.LocalTime
import           Date


data DateTime = DateTime { _date :: Date, _time :: TimeOfDay }

toDateTime :: LocalTime -> DateTime
toDateTime lt = DateTime (toDate lt) (localTimeOfDay lt)
