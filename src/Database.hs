module Database where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Time

-- import           Data.Time.LocalTime
import           Protolude


makeConnection :: IO Connection
makeConnection = connect defaultConnectInfo { connectDatabase = "calendar" }

respond moves color n = do
    conn <- makeConnection
    let q = "select * from day"
        params = ()
    res <- query conn q params :: IO [(Int, Date)]
    print res
