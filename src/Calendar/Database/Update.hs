module Calendar.Database.Update where

import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple
import           Protolude

import           Calendar.Data.Day
import           Calendar.Data.Entry
import qualified Calendar.Database.Sql      as SQL


type EntryID = Int
type DayID   = Int


-------------------------------------------------------------------------------
-- Update


updateEntry :: Connection -> EntryID -> Maybe TimeOfDay -> Text -> Bool -> IO Int64
updateEntry conn id ts desc isdone =
    execute conn SQL.updateEntry (ts, desc, isdone, id)


entryDone :: Connection -> EntryID -> IO Int64
entryDone conn id = do
    _   <- execute conn SQL.updateDone (Only id)
    return 1


-------------------------------------------------------------------------------
-- Insert


insertDay :: Connection -> Day -> IO Int64
insertDay conn (Day id d es) = do
    _ <- execute conn SQL.insertDay (Only d)
    insertEntries conn id es


insertEntry :: Connection -> DayID -> Entry -> IO Int64
insertEntry conn id (Entry _ ts desc isdone) =
    execute conn SQL.insertEntry (id, ts, desc, isdone)


insertEntries :: Connection -> DayID -> [Entry] -> IO Int64
insertEntries conn did es = executeMany conn SQL.insertEntry values
    where
        entryToTuple (Entry eid t desc isdone) = (eid, did, t, desc, isdone)
        values = map entryToTuple es


-------------------------------------------------------------------------------
-- Delete


deleteEntry :: Connection -> EntryID -> IO Int64
deleteEntry conn id = execute conn SQL.deleteEntry (Only id)


-------------------------------------------------------------------------------
-- Todo

insertTodo :: Connection -> Text -> IO Int64
insertTodo conn t =
     execute conn SQL.insertTodo (t, False)

