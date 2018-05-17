module Calendar.Data.Day where

import           Data.Time.Calendar  as T
import           Data.Time.LocalTime
import           Data.Time.Format
import           Protolude
import Data.Text (pack)

import           Calendar.Data.Entry

type Date = T.Day

data Day = Day Int Date [Entry] deriving (Show)

dayFormat :: T.Day -> Text
dayFormat = pack . formatTime defaultTimeLocale "%A %F"

timeFormat :: TimeOfDay -> Text
timeFormat = pack . formatTime defaultTimeLocale "%R"
