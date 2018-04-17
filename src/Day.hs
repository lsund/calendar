module Day where

import           Protolude hiding (Show, show)
import           GHC.Show

import           Time
import           Prelude                       (String)


data Date = Date
    { _year  :: Int
    , _month :: Int
    , _day   :: Int
    } deriving (Show)

data Entry = Entry { _time :: Time, _desc :: String, _done :: Bool }

instance Show Entry where
    show (Entry t desc _) = show t ++ " " ++ desc

data Day = Day Date [Entry] deriving (Show)

