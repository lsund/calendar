module Writer where


import Protolude

import qualified Data.Text as T

import CalendarDay
import Date


dayToString :: CalendarDay -> Text
dayToString (CalendarDay d es) = show d <> "\n\n" <> T.intercalate "\n"
                            (map (\e ->
                                    if _done e
                                        then "D " <> show e
                                        else "T " <> show e) es)

serialize :: CalendarDay -> Entry -> IO ()
serialize (CalendarDay d es) e =
    writeFile (dateToPath d) (dayToString (CalendarDay d (e : es)))
