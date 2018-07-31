module Calendar.Data.Todo where

import Protolude

data TODO = TODO {_id :: Int, _desc :: Text, _done :: Bool }
