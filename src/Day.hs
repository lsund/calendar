module Day where

import           Data.Text (unpack)
import           GHC.Show  (Show, show)
import           Protolude hiding (show)

import           Date
import           Time

data Entry = Entry { _time :: Time, _desc :: Text, _done :: Bool }

data Day = Day Date [Entry] deriving (Show)

instance Show Entry where
    show (Entry t desc _) = show t <> " " <> unpack desc
