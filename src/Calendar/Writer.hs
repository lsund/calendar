module Calendar.Writer where


import           Protolude

import qualified Data.Text   as T

import           Calendar.Day
import           Calendar.Date


dayToString :: CalendarDay -> Text
dayToString (CalendarDay _ d es) = show d <> "\n\n" <> T.intercalate "\n"
                            (map (\e ->
                                    if _done e
                                        then "D " <> show e
                                        else "T " <> show e) es)

serialize :: CalendarDay -> Entry -> IO ()
serialize (CalendarDay id d es) e =
    writeFile (dateToPath d) (dayToString (CalendarDay id d (e : es)))
