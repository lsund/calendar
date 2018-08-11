module Calendar.Database.Query where

import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple
import           Protolude

import           Calendar.Data.Day
import           Calendar.Data.Entry
import           Calendar.Data.Todo
import           Calendar.Database.Internal
import           Calendar.Database.Update


type EntryTuple = (Int, Int, Maybe TimeOfDay, Text, Bool)


getDay :: Date -> IO Day
getDay date = do
    conn <- makeConnection
    ids <- query conn idQ (Only date) :: IO [Only Int]
    if null ids
        then do
            _ <- insertDay conn (Day 0 date [])
            getDay date
        else do
            let (id : _) = ids
            entries <- query conn entryQ id :: IO [EntryTuple]
            return $ Day (fromOnly id) date $ map makeEntry entries
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
