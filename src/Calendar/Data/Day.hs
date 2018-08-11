module Calendar.Data.Day where

import qualified Data.Time.Calendar  as DTC
import           Data.Time.LocalTime
import           Data.Time.Format
import           Protolude
import Data.Text (pack)
import Text.Printf

import           Calendar.Data.Entry

type Date = DTC.Day

data Day = Day { _id :: Int, _date :: Date, _entries :: [Entry] } deriving (Show)

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
