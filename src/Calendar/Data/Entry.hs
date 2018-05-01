module Calendar.Data.Entry where

import qualified Data.Text           as T
import           Data.Time.LocalTime
import           GHC.Show            (Show, show)
import           Protolude           hiding (show)

data Entry = Entry { _id :: Int,  _time :: TimeOfDay, _desc :: Text, _done :: Bool }

instance Show Entry where
    show (Entry _ t desc _) = take 5 (show t) <> " " <> T.unpack desc

sortEntries :: [Entry] -> [Entry]
sortEntries = sortBy (\x y -> _time x `compare` _time y)

