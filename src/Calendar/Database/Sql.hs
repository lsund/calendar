module Calendar.Database.Sql where

import           Database.PostgreSQL.Simple

updateEntry :: Query
updateEntry = "update entry set ts=?, description=?, done=? where id=?"

updateDone :: Query
updateDone = "update entry set done=true where id=?"

-- TODO This does not work. Increment ID does not mean that the date gets
-- incremented by one
push :: Query
push = "update entry set dayid=dayid+1 where id=?"


insertEntry :: Query
insertEntry = "insert into entry (dayid, ts, description, done) values (?,?,?,?)"


insertDay :: Query
insertDay = "insert into day (gregorian) values (?)"


deleteEntry :: Query
deleteEntry = "delete from entry where id=?"


insertTodo :: Query
insertTodo = "insert into todoentry (description, done) values (?, ?)"


removeTodo :: Query
removeTodo = "delete from todoentry where id=?"


updateTodo :: Query
updateTodo = "update todoentry set description=? where id=?"
