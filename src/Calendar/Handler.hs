module Calendar.Handler where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Time.LocalTime
import           Protolude                  hiding (get)
import           Web.Spock
import           Database.PostgreSQL.Simple

import           Calendar.Data.Entry
import           Calendar.Database.Query
import           Calendar.Database.Update
import qualified Calendar.Renderer          as R


type Server a = SpockM () () () a

nfiles :: Int
nfiles = 7


rootGET :: Connection -> Server ()
rootGET _ =
    get root $ do

        d <- (localDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
        tod <- (localTimeOfDay . zonedTimeToLocalTime) <$> liftIO getZonedTime

        let dates = take 14 $ iterate succ d
        days <- liftIO $ mapM getDay dates
        R.index tod days



addPOST :: Connection -> Server ()
addPOST conn =
    post "add" $ do
        h                <- param' "hour"
        m                <- param' "minute"
        desc             <- param' "desc"
        id                <- param' "id"
        let e = Entry 0 (TimeOfDay h m 0) desc False

        _ <- liftIO $ insertEntry conn id e
        redirect "/"


updatePOST :: Connection -> Server ()
updatePOST conn =
    post "update" $ do
        t <- param' "time"
        -- d <- param' "day"
        desc <- param' "desc"
        id <- param' "id"
        done <- param' "done"

        _ <- liftIO $ updateEntry conn id t desc done

        redirect "/"


donePOST :: Connection -> Server ()
donePOST conn =
    post "done" $ do
        id <- param' "id"

        _ <- liftIO $ entryDone conn id

        redirect "/"
