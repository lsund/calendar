module Database where

import           Database.PostgreSQL.Simple
-- import           Database.PostgreSQL.Simple.Time
import           Data.Time.LocalTime

import           Day
import           Protolude

type EntryRow = (Int, Int, TimeOfDay, Text, Bool)

makeConnection :: IO Connection
makeConnection = connect defaultConnectInfo { connectDatabase = "calendar" }

rowToEntry :: EntryRow -> Entry
rowToEntry (_, _, tod, desc, isdone) = Entry tod desc isdone

-- insertEntries :: Connection -> Int -> [Entry] -> IO Int64
-- insertEntries conn dayid es =
--     executeMany conn q values
--     where
--         q = "insert into entry (dayid, ts, description, done) values (?,?,?,?,?)"
--         entryToTuple (Entry t desc done) = (dayid, t, desc, done)
--         values = map entryToTuple es

respond :: IO ()
respond = do
    conn <- makeConnection
    let entryQ = "select * from entry where dayid=1"
        params = ()
    entries <- query conn entryQ params :: IO [EntryRow]
    print (map rowToEntry entries)
