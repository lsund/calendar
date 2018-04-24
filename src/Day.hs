module Day where

import           Data.Text (unpack)
import           GHC.Show  (Show, show)
import           Protolude hiding (show)
import           Data.Time.LocalTime

import           Date

data Entry = Entry { _time :: TimeOfDay, _desc :: Text, _done :: Bool }

data Day = Day Date [Entry] deriving (Show)

instance Show Entry where
    show (Entry t desc _) = show t <> " " <> unpack desc
