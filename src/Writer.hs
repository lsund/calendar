module Writer where


import Protolude

import Date
import Time


line :: Date -> Int -> Int -> Text -> Text
line _ h m desc = "T " <> show (Time h m) <> " " <> desc <> "\n"

writeToFile :: Date -> Int -> Int -> Text -> IO ()
writeToFile d h m desc = appendFile (dateToPath d) (line d h m desc)
