module Calendar.Data.Day where

import           Data.Text                   (pack)
import qualified Data.Time.Calendar          as DTC
import           Data.Time.Calendar.WeekDate
import           Data.Time.Format
import           Data.Time.LocalTime
import           Protolude
import           Text.Printf

import           Calendar.Data.Entry

type Date = DTC.Day

data Day = Day { _id :: Int, _date :: Date, _entries :: [Entry] } deriving (Show)

dayOfWeek :: Integer -> Int -> Int -> Int
dayOfWeek day month year =
    let (_, _, d) = toWeekDate $ DTC.fromGregorian day month year
    in d

dateFormat :: Date -> Text
dateFormat = pack . formatTime defaultTimeLocale "%A %F"

timeFormat :: TimeOfDay -> Text
timeFormat = pack . formatTime defaultTimeLocale "%R"

dateURL :: Date -> Text
dateURL date =
    let (year, month, day) = DTC.toGregorian date
    in pack $ printf "/%d/%d/%d" year month day

dayURL :: Day -> Text
dayURL d = pack $ printf "/%d/%d/%d" year month day
    where (year, month, day) = DTC.toGregorian (_date d)

makeURL :: Integer -> Int -> Int -> Text
makeURL year month day = pack $ printf "/%d/%d/%d" year month day
