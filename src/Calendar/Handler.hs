module Calendar.Handler where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Time
import           Data.Time.Calendar  as T
-- import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple
import           Protolude
import           Web.Spock                  ((<//>), var)
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


getDay :: Connection -> Server ()
getDay _ =
    S.get (var <//> var <//> var) $ \year month day -> do
        let calenderDay = T.fromGregorian year month day
        tod <- (localTimeOfDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
        day <- liftIO $ DBQ.getDay calenderDay
        todos <- liftIO DBQ.getTodos
        R.day day tod Nothing todos


daysAndTime :: SpockContext ([D.Day], TimeOfDay)
daysAndTime = do
    d   <- (localDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
    tod <- (localTimeOfDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
    let dates = take nfiles $ iterate succ d
    days <- liftIO $ mapM DBQ.getDay dates
    return (days, tod)


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
        dayid   <- S.param' "dayid"
        let e = Entry 0 Nothing desc False

        _ <- liftIO $ DBU.insertEntry conn dayid e
        S.redirect $ makeQueryString "/day" ("id", show dayid)


update :: Connection -> Server ()
update conn =
    S.post "update" $ do
        t    <- S.param' "time"
        desc <- S.param' "desc"
        dayid <- S.param' "dayid"
        entryid <- S.param' "entryid"
        isdone <- S.param' "done"
        let mt = parseMaybeTime t
        print desc
        _ <- liftIO $ DBU.updateEntry conn entryid mt desc isdone
        S.redirect $ makeQueryString "/day" ("id", show (dayid :: Int))


done :: Connection -> Server ()
done conn =
    S.post "done" $ do
        entryid <- S.param' "entryid"
        dayid <- S.param' "dayid"
        _ <- liftIO $ DBU.entryDone conn entryid
        S.redirect $ makeQueryString "/day" ("id", show (dayid :: Int))


delete :: Connection -> Server ()
delete conn =
    S.post "delete" $ do
        entryid <- S.param' "entryid"
        dayid <- S.param' "dayid"
        _ <- liftIO $ DBU.deleteEntry conn entryid
        S.redirect $ makeQueryString "/day" ("id", show (dayid :: Int))


push :: Connection -> Server ()
push conn =
    S.post "push" $ do
        entryid <- S.param' "entryid"
        dayid <- S.param' "dayid"
        _ <- liftIO $ DBU.push conn entryid
        S.redirect $ makeQueryString "/day" ("id", show (dayid :: Int))


addTodo :: Connection -> Server ()
addTodo conn =
    S.post "add-todo" $ do
        dayid <- S.param' "dayid"
        t <- S.param' "desc"
        _ <- liftIO $ DBU.insertTodo conn t
        S.redirect $ makeQueryString "/day" ("id", show (dayid :: Int))


removeTodo :: Connection -> Server ()
removeTodo conn =
    S.post "remove-todo" $ do
        todoid <- S.param' "todoid"
        dayid <- S.param' "dayid"
        _ <- liftIO $ DBU.removeTodo conn todoid
        S.redirect $ makeQueryString "/day" ("id", show (dayid :: Int))


updateTodo :: Connection -> Server ()
updateTodo conn =
    S.post "update-todo" $ do
        todoid <- S.param' "todoid"
        dayid <- S.param' "dayid"
        desc <- S.param' "desc"
        _ <- liftIO $ DBU.updateTodo conn todoid desc
        S.redirect $ makeQueryString "/day" ("id", show (dayid :: Int))
