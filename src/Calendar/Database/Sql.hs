module Calendar.Database.Sql where

import           Database.PostgreSQL.Simple

updateEntry :: Query
updateEntry = "update entry set ts=?, description=?, done=? where id=?"


updateDone :: Query
updateDone = "update entry set done=true where id=?"


insertEntry :: Query
insertEntry = "insert into entry (dayid, ts, description, done) values (?,?,?,?)"


insertDay :: Query
insertDay = "insert into day (gregorian) values (?)"
