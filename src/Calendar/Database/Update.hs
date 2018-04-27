module Calendar.Database.Update where

import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple
import           Protolude

import           Calendar.Data.Day
import           Calendar.Data.Entry

updateEntry :: Connection -> Int -> TimeOfDay -> Text -> Bool -> IO Int64
updateEntry conn id ts desc isdone = do
    _                <- execute conn updateQ (ts, desc, isdone, id)
    return 1
    where
        updateQ = "update entry set ts=?, description=?, done=? where id=?"

insertEntries :: Connection -> Int -> [Entry] -> IO Int64
insertEntries conn dayid es =
    executeMany conn q values
    where
        q = "insert into entry (dayid, ts, description, done) values (?,?,?,?)"
        entryToTuple (Entry id t desc isdone) = (id, dayid, t, desc, isdone)
        values = map entryToTuple es


insertDay :: Connection -> Day -> IO Int64
insertDay conn (Day id d es) = do
    _                <- execute conn insertQ (Only d)
    insertEntries conn id es
    where
        insertQ = "insert into day (gregorian) values (?)"


entryDone :: Connection -> Int -> IO Int64
entryDone conn id = do
    _   <- execute conn updateQ (Only id)
    return 1
    where
        updateQ = "update entry set done=true where id=?"


addEntry :: Connection -> Int -> Entry -> IO Int64
addEntry conn dayid (Entry _ ts desc isdone) =
    execute conn insertQ (dayid, ts, desc, isdone)
    where
        insertQ = "insert into entry (dayid, ts, description, done) values (?,?,?,?)"


