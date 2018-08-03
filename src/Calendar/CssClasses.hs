module Calendar.CssClasses where

import Protolude
import           Data.Time.LocalTime

import           Calendar.Data.Entry

entry :: Entry -> TimeOfDay -> Text
entry e _
    | _done e             = "done mui--divider-bottom"
    | otherwise           = "mui-textfield"


button :: Text
-- button = ""
button = "submit mui-btn"


form :: Text
form = "mui-form--inline"


time :: Text
time = "time mui-textfield"


desc :: Text
desc = "desc mui-textfield"
