module Calendar.CssClasses where

import Protolude
import           Data.Time.LocalTime

import           Calendar.Data.Entry

entry :: Entry -> TimeOfDay -> Text
entry e _
    | _done e             = ""
    | otherwise           = ""


button :: Text
-- button = ""
button = ""


form :: Text
form = ""


time :: Text
time = "time"


desc :: Text
desc = "desc"
