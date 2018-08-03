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
import qualified Calendar.Database.Query    as DBQ
import qualified Calendar.Database.Update   as DBU
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
    days <- liftIO $ mapM DBQ.getDay dates
    return (days, tod)


getDay :: Connection -> Server ()
getDay _ =
    S.get "day" $ do
        x <- S.param' "id"
        d   <- (localDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
        tod <- (localTimeOfDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
        day <- liftIO $ DBQ.getDay (addDays x d)
        todos <- liftIO DBQ.getTodos
        R.day tod day  Nothing todos


getRoot :: Connection -> Server ()
getRoot _ =
    S.get S.root $ do
        (days, tod) <- daysAndTime
        todos <- liftIO DBQ.getTodos
        R.index tod days [] todos


getWeather :: Connection -> Server ()
getWeather _ =
    S.get "weather" $ do
        (Just fc) <- liftIO getForecast
        (days, tod) <- daysAndTime
        todos <- liftIO DBQ.getTodos
        R.index tod days fc todos


add :: Connection -> Server ()
add conn =
    S.post "add" $ do
        desc <- S.param' "desc"
        id   <- S.param' "id"
        let e = Entry 0 Nothing desc False

        _ <- liftIO $ DBU.insertEntry conn id e
        S.redirect "/"


update :: Connection -> Server ()
update conn =
    S.post "update" $ do
        t    <- S.param' "time"
        desc <- S.param' "desc"
        id   <- S.param' "id"
        isdone <- S.param' "done"

        print t

        let mt = parseMaybeTime t

        _ <- liftIO $ DBU.updateEntry conn id mt desc isdone

        S.redirect "/"


done :: Connection -> Server ()
done conn =
    S.post "done" $ do
        id <- S.param' "id"
        _ <- liftIO $ DBU.entryDone conn id

        S.redirect "/"


delete :: Connection -> Server ()
delete conn =
    S.post "delete" $ do
        id <- S.param' "id"
        _ <- liftIO $ DBU.deleteEntry conn id

        S.redirect "/"


addTodo :: Connection -> Server ()
addTodo conn =
    S.post "add-todo" $ do
        t <- S.param' "desc"
        _ <- liftIO $ DBU.insertTodo conn t
        S.redirect "/"


removeTodo :: Connection -> Server ()
removeTodo conn =
    S.post "remove-todo" $ do
        id <- S.param' "id"
        _ <- liftIO $ DBU.removeTodo conn id
        S.redirect "/"


push :: Connection -> Server ()
push conn =
    S.post "push" $ do
        id <- S.param' "id"
        _ <- liftIO $ DBU.push conn id
        S.redirect "/"
