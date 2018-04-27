module Calendar.File.Writer where


import           Protolude

import qualified Data.Text   as T

import           Calendar.Day
import           Calendar.Date


dayToString :: Day -> Text
dayToString (Day _ d es) = show d <> "\n\n" <> T.intercalate "\n"
                            (map (\e ->
                                    if _done e
                                        then "D " <> show e
                                        else "T " <> show e) es)

serialize :: Day -> Entry -> IO ()
serialize (Day id d es) e =
    writeFile (dateToPath d) (dayToString (Day id d (e : es)))
