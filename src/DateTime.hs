module DateTime where

import           Data.Time.LocalTime
import           Date
import           Time


data DateTime = DateTime { _date :: Date, _time :: Time }

toDateTime :: LocalTime -> DateTime
toDateTime lt = DateTime (toDate lt) (toTime lt)
