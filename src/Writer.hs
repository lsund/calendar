module Writer where


import Protolude

import qualified Data.Text as T

import Day
import Date


dayToString :: Day -> Text
dayToString (Day d es) = show d <> "\n\n" <> T.intercalate "\n"
                            (map (\e ->
                                    if _done e
                                        then "D " <> show e
                                        else "T " <> show e) es)

serialize :: Day -> Entry -> IO ()
serialize (Day d es) e = writeFile (dateToPath d) (dayToString (Day d (e : es)))
