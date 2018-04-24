module Database where

import           Database.PostgreSQL.Simple
-- import           Database.PostgreSQL.Simple.Time
import           Data.Time.Calendar
import           Data.Time.LocalTime

import           CalendarDay
import           DateTime
import           Parser
import           Protolude

type EntryRow = (Int, Int, TimeOfDay, Text, Bool)

makeConnection :: IO Connection
makeConnection = connect defaultConnectInfo { connectDatabase = "calendar" }

rowToEntry :: EntryRow -> Entry
rowToEntry (_, _, tod, desc, isdone) = Entry tod desc isdone


insertEntries :: Connection -> Int -> [Entry] -> IO Int64
insertEntries conn dayid es =
    executeMany conn q values
    where
        q = "insert into entry (dayid, ts, description, done) values (?,?,?,?)"
        entryToTuple (Entry t desc isdone) = (dayid, t, desc, isdone)
        values = map entryToTuple es


insertDay :: Connection -> Only Day -> IO Int64
insertDay conn = execute conn q
    where
        q = "insert into day (gregorian) values (?)"


respond :: IO ()
respond = do
    conn <- makeConnection
    let entryQ = "select * from entry where dayid=1"
        params = ()
    entries <- query conn entryQ params :: IO [EntryRow]

    (ZonedTime lt _) <- liftIO getZonedTime
    let (DateTime d t) = toDateTime lt
    days@(Right (CalendarDay d es) : xs) <- liftIO $ readDays d 5

    if any isLeft days
        then print ("Could not parse a file" :: Text)
        else do
            _ <- insertDay conn (Only (ModifiedJulianDay 1))
            x <- insertEntries conn 1 es
            print x

    print (map rowToEntry entries)
