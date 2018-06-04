module Calendar.Handler where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple
import           Protolude
import qualified Web.Spock                  as S

import           Calendar.Data.Entry
import           Calendar.Database.Query
import           Calendar.Database.Update
import qualified Calendar.Renderer          as R
import           Calendar.Forecast


type Server a = S.SpockM () () () a

nfiles :: Int
nfiles = 45

daysAndTime = do
    d   <- (localDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
    tod <- (localTimeOfDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
    let dates = take nfiles $ iterate succ d
    days <- liftIO $ mapM getDay dates
    return (days, tod)

getRootWeather :: Connection -> Server ()
getRootWeather _ =
    S.get "weather" $ do

        (Just fc) <- liftIO getForecast

        (days, tod) <- daysAndTime

        R.index tod days fc

getRoot :: Connection -> Server ()
getRoot _ =
    S.get S.root $ do

        (days, tod) <- daysAndTime

        R.index tod days []


add :: Connection -> Server ()
add conn =
    S.post "add" $ do
        time <- S.param' "time"
        desc <- S.param' "desc"
        id   <- S.param' "id"
        let e = Entry 0 time desc False

        _ <- liftIO $ insertEntry conn id e
        S.redirect "/"


update :: Connection -> Server ()
update conn =
    S.post "update" $ do
        t    <- S.param' "time"
        desc <- S.param' "desc"
        id   <- S.param' "id"
        isdone <- S.param' "done"

        _ <- liftIO $ updateEntry conn id t desc isdone

        S.redirect "/"


done :: Connection -> Server ()
done conn =
    S.post "done" $ do
        id <- S.param' "id"
        _ <- liftIO $ entryDone conn id

        S.redirect "/"


delete :: Connection -> Server ()
delete conn =
    S.post "delete" $ do
        id <- S.param' "id"
        _ <- liftIO $ deleteEntry conn id

        S.redirect "/"
