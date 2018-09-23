module Calendar.Data.Day where

import           Data.Text                   (pack)
import qualified Data.Time.Calendar          as DTC
import           Data.Time.Format
import           Data.Time.LocalTime
import           Protolude
import           Text.Printf

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


dateMonth :: Date -> Int
dateMonth date =
    let (_, m, _) = DTC.toGregorian date
    in m


subMonth :: Date -> Date
subMonth date =
    let (y, m, d) = DTC.toGregorian date
    in DTC.fromGregorian y (pred m) d


addMonth :: Date -> Date
addMonth date =
    let (y, m, d) = DTC.toGregorian date
    in DTC.fromGregorian y (succ m) d

