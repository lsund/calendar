module Calendar.Database.Internal where

import           Database.PostgreSQL.Simple
import           Protolude

makeConnection :: IO Connection
makeConnection = connect defaultConnectInfo { connectDatabase = "calendar" }
