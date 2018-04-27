module Calendar.Data.Day where

import           Data.Text           (unpack)
import           Data.Time.Calendar as T
import           Data.Time.Format
import           Data.Time.LocalTime
import           GHC.Show            (Show, show)
import           Prelude             (String)
import           Protolude           hiding (show)

type Date = T.Day

data Entry = Entry { _id :: Int,  _time :: TimeOfDay, _desc :: Text, _done :: Bool }

data Day = Day Int Date [Entry] deriving (Show)

instance Show Entry where
    show (Entry _ t desc _) = show t <> " " <> unpack desc

sortEntries :: [Entry] -> [Entry]
sortEntries = sortBy (\x y -> _time x `compare` _time y)

dayFormat :: T.Day -> String
dayFormat = formatTime defaultTimeLocale "%A %F"

timeFormat :: TimeOfDay -> String
timeFormat = formatTime defaultTimeLocale "%R"
