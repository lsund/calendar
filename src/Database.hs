module Database where

import           Database.PostgreSQL.Simple
-- import           Database.PostgreSQL.Simple.Time
import           Data.Time.Calendar
import           Data.Time.LocalTime

import           CalendarDay
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


insertDay :: Connection -> CalendarDay -> IO Int64
insertDay conn (CalendarDay d es) = do
    _                <- execute conn insertQ (Only d)
    (Only dayid : _) <- query conn idQ (Only d)
    insertEntries conn dayid es
    where
        insertQ = "insert into day (gregorian) values (?)"
        idQ = "select id from day where gregorian=?"

-- start at 14th april, 55 days
insertDays :: Day -> Int -> IO ()
insertDays start n = do
    conn <- makeConnection
    days <- liftIO $ readDays start n

    if any isLeft days
        then print ("Could not parse a file" :: Text)
        else do
            x <- mapM_ (insertDay conn) (rights days)
            print x


getDay :: Day -> IO CalendarDay
getDay d = do
    conn <- makeConnection
    (Only dayid : _) <- query conn idQ (Only d) :: IO [Only Int]
    res <- query conn "select * from entry where dayid=?" (Only dayid) :: IO [(Int, Int, TimeOfDay, Text, Bool)]
    return $ CalendarDay d $ map makeEntry res
    where
        idQ = "select id from day where gregorian=?"
        makeEntry (_, _, tod, desc, isdone) = Entry tod desc isdone

