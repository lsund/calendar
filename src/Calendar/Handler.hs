module Calendar.Handler where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Time
-- import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple
import           Protolude
import qualified Web.Spock                  as S

import           Calendar.Config
import           Calendar.Data.Day          as D
import           Calendar.Data.Entry
import           Calendar.Database.Query
import           Calendar.Database.Update
import           Calendar.Forecast
import qualified Calendar.Renderer          as R
import           Calendar.Util

type Server a = S.SpockM () () () a
type SpockState = S.WebStateM () () ()
type SpockContext a = S.ActionCtxT () SpockState a


daysAndTime :: SpockContext ([D.Day], TimeOfDay)
daysAndTime = do
    d   <- (localDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
    tod <- (localTimeOfDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
    let dates = take nfiles $ iterate succ d
    days <- liftIO $ mapM getDay dates
    return (days, tod)


getRoot :: Connection -> Server ()
getRoot _ =
    S.get S.root $ do

        (days, tod) <- daysAndTime

        R.index tod days []


getWeather :: Connection -> Server ()
getWeather _ =
    S.get "weather" $ do
        (Just fc) <- liftIO getForecast
        (days, tod) <- daysAndTime
        R.index tod days fc


add :: Connection -> Server ()
add conn =
    S.post "add" $ do
        desc <- S.param' "desc"
        id   <- S.param' "id"
        let e = Entry 0 Nothing desc False

        _ <- liftIO $ insertEntry conn id e
        S.redirect "/"


update :: Connection -> Server ()
update conn =
    S.post "update" $ do
        t    <- S.param' "time"
        desc <- S.param' "desc"
        id   <- S.param' "id"
        isdone <- S.param' "done"

        let mt = parseMaybeTime t

        _ <- liftIO $ updateEntry conn id mt desc isdone

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
