module Calendar.Data.Day where

import           Data.Time.Calendar  as DTC
import           Data.Time.LocalTime
import           Data.Time.Format
import           Protolude
import Data.Text (pack)
import Text.Printf

import           Calendar.Data.Entry

type Date = DTC.Day

data Day = Day { _id :: Int, _date :: Date, _entries :: [Entry] } deriving (Show)

dayFormat :: Date -> Text
dayFormat = pack . formatTime defaultTimeLocale "%A %F"

timeFormat :: TimeOfDay -> Text
timeFormat = pack . formatTime defaultTimeLocale "%R"

dateURL :: Date -> Text
dateURL date =
    let (year, month, day) = DTC.toGregorian date
    in pack $ printf "/%d/%d/%d" year month day
