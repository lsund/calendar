module Calendar.Database.Query where

import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple
import           Protolude

import           Calendar.Data.Entry
import           Calendar.Data.Day
import           Calendar.Database.Internal
import           Calendar.Database.Update


getDay :: Date -> IO Day
getDay d = do
    conn <- makeConnection
    ids <- query conn idQ (Only d) :: IO [Only Int]
    if null ids
        then do
            _ <- insertDay conn (Day 0 d [])
            getDay d
        else do
            let (id : _) = ids
            res <- query conn entryQ  id :: IO [(Int, Int, TimeOfDay, Text, Bool)]
            return $ Day (fromOnly id) d $ map makeEntry res
            where
                idQ = "select id from day where gregorian=?"
                entryQ = "select * from entry where dayid=?"
                makeEntry (id, _, tod, desc, isdone) = Entry id tod desc isdone
