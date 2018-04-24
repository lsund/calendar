module CalendarDay where

import           Data.Text           (unpack)
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           GHC.Show            (Show, show)
import           Protolude           hiding (show)

data Entry = Entry { _time :: TimeOfDay, _desc :: Text, _done :: Bool }

data CalendarDay = CalendarDay Day [Entry] deriving (Show)

instance Show Entry where
    show (Entry t desc _) = show t <> " " <> unpack desc
