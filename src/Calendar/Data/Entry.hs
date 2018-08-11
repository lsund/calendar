module Calendar.Data.Entry where

import qualified Data.Text           as T
import           Data.Time.LocalTime
import           GHC.Show            (Show, show)
import           Protolude           hiding (show)

data Entry = Entry { _id :: Int,  _time :: Maybe TimeOfDay, _desc :: Text, _done :: Bool }

instance Show Entry where
    show (Entry _ t desc _) = take 5 (show t) <> " " <> T.unpack desc

sort :: [Entry] -> [Entry]
sort = sortBy (\x y -> _time x `compare` _time y)

