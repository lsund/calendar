module Calendar.Database.Internal where

import           Database.PostgreSQL.Simple
import           Data.Time.LocalTime
import           Protolude

import           Calendar.Day
import           Calendar.File.Parser

type EntryRow = (Int, Int, TimeOfDay, Text, Bool)

makeConnection :: IO Connection
makeConnection = connect defaultConnectInfo { connectDatabase = "calendar" }

rowToEntry :: EntryRow -> Entry
rowToEntry (id, _, tod, desc, isdone) = Entry id tod desc isdone


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

-- start at 14th april, 55 days
insertDays :: Date -> Int -> IO ()
insertDays start n = do
    conn <- makeConnection
    days <- liftIO $ readDays start n

    if any isLeft days
        then print ("Could not parse a file" :: Text)
        else do
            x <- mapM_ (insertDay conn) (rights days)
            print x


updateEntry :: Int -> TimeOfDay -> Text -> Bool -> IO Int64
updateEntry id ts desc isdone = do
    conn <- makeConnection
    _                <- execute conn updateQ (ts, desc, isdone, id)
    return 1
    where
        updateQ = "update entry set ts=?, description=?, done=? where id=?"



entryDone :: Int -> IO Int64
entryDone id = do
    conn <- makeConnection
    _   <- execute conn updateQ (Only id)
    return 1
    where
        updateQ = "update entry set done=true where id=?"


addEntry :: Int -> Entry -> IO Int64
addEntry dayid (Entry _ ts desc isdone) = do
    conn <- makeConnection
    execute conn insertQ (dayid, ts, desc, isdone)
    where
        insertQ = "insert into entry (dayid, ts, description, done) values (?,?,?,?)"


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

