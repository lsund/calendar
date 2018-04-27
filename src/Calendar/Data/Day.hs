module Calendar.Data.Day where

import           Data.Time.Calendar  as T
import           Data.Time.LocalTime
import           Data.Time.Format
import           Prelude             (String)
import           Protolude

import           Calendar.Data.Entry

type Date = T.Day

data Day = Day Int Date [Entry] deriving (Show)

dayFormat :: T.Day -> String
dayFormat = formatTime defaultTimeLocale "%A %F"

timeFormat :: TimeOfDay -> String
timeFormat = formatTime defaultTimeLocale "%R"
