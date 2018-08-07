module Calendar.Database.Query where

import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple
import           Protolude

import           Calendar.Data.Day
import           Calendar.Data.Entry
import           Calendar.Data.Todo
import           Calendar.Database.Internal
import           Calendar.Database.Update


getDayFromID :: Int -> IO (Maybe Day)
getDayFromID id = do
    conn <- makeConnection
    dates <- query conn dayQ (Only id) :: IO [Only Date]
    if null dates then
        return Nothing
    else do
        es <- query conn entryQ (Only id) :: IO [(Int, Int, Maybe TimeOfDay, Text, Bool)]
        let (date : _) = dates
        return $ Just $ Day id (fromOnly date) $ map makeEntry es
    where
        dayQ = "select gregorian from day where id=?"
        entryQ = "select * from entry where dayid=?"
        makeEntry (entryid, _, tod, desc, isdone) =
            Entry entryid tod desc isdone


getDayfromDate :: Date -> IO Day
getDayfromDate date = do
    conn <- makeConnection
    ids <- query conn idQ (Only date) :: IO [Only Int]
    if null ids
        then do
            _ <- insertDay conn (Day 0 date [])
            getDayfromDate date
        else do
            let (id : _) = ids
            res <- query conn entryQ  id :: IO [(Int, Int, Maybe TimeOfDay, Text, Bool)]
            return $ Day (fromOnly id) date $ map makeEntry res
            where
                idQ = "select id from day where gregorian=?"
                entryQ = "select * from entry where dayid=?"
                makeEntry (id, _, tod, desc, isdone) = Entry id tod desc isdone



getTodos :: IO [TODO]
getTodos = do
    conn <- makeConnection
    todos <- query_ conn todoQ :: IO [(Int, Text, Bool)]
    return $ map makeTodo todos
    where
        todoQ = "select * from TodoEntry where done=false"
        makeTodo (id, desc, isdone) = TODO id desc isdone
