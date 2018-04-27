module Calendar.Handler where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Time.LocalTime
import           Protolude                  hiding (get)
import           Web.Spock

import           Calendar.Data.Entry
import           Calendar.Database.Internal
import           Calendar.Database.Query
import           Calendar.Database.Update
import qualified Calendar.Renderer          as R


type Server a = SpockM () () () a

nfiles :: Int
nfiles = 7


rootGET :: Server ()
rootGET =
    get root $ do

        d <- (localDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
        tod <- (localTimeOfDay . zonedTimeToLocalTime) <$> liftIO getZonedTime

        let dates = take 4 $ iterate succ d
        days <- liftIO $ mapM getDay dates
        R.index tod days



addPOST :: Server ()
addPOST =
    post "add" $ do
        h                <- param' "hour"
        m                <- param' "minute"
        desc             <- param' "desc"
        id                <- param' "id"
        let e = Entry 0 (TimeOfDay h m 0) desc False

        conn <- liftIO makeConnection
        _ <- liftIO $ addEntry conn id e
        redirect "/"


updatePOST :: Server ()
updatePOST =
    post "update" $ do
        t <- param' "time"
        -- d <- param' "day"
        desc <- param' "desc"
        id <- param' "id"
        done <- param' "done"

        conn <- liftIO makeConnection
        _ <- liftIO $ updateEntry conn id t desc done

        redirect "/"


donePOST :: Server ()
donePOST =
    post "done" $ do
        id <- param' "id"

        conn <- liftIO makeConnection
        _ <- liftIO $ entryDone conn id

        redirect "/"
