module Calendar.Database.Update where

import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple
import           Protolude

import           Calendar.Data.Day
import           Calendar.Data.Entry
import qualified Calendar.Database.Sql      as SQL


-------------------------------------------------------------------------------
-- Update


updateEntry :: Connection -> Int -> TimeOfDay -> Text -> Bool -> IO Int64
updateEntry conn id ts desc isdone =
    execute conn SQL.updateEntry (ts, desc, isdone, id)


entryDone :: Connection -> Int -> IO Int64
entryDone conn id = do
    _   <- execute conn SQL.updateDone (Only id)
    return 1


-------------------------------------------------------------------------------
-- Insert


insertDay :: Connection -> Day -> IO Int64
insertDay conn (Day id d es) = do
    _ <- execute conn SQL.insertDay (Only d)
    insertEntries conn id es


insertEntry :: Connection -> Int -> Entry -> IO Int64
insertEntry conn dayid (Entry _ ts desc isdone) =
    execute conn SQL.insertEntry (dayid, ts, desc, isdone)


insertEntries :: Connection -> Int -> [Entry] -> IO Int64
insertEntries conn dayid es = executeMany conn SQL.insertEntry values
    where
        entryToTuple (Entry id t desc isdone) = (id, dayid, t, desc, isdone)
        values = map entryToTuple es


