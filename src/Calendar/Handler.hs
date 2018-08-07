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
    days <- liftIO $ mapM DBQ.getDayfromDate dates
    return (days, tod)


getDay :: Connection -> Server ()
getDay _ =
    S.get "day" $ do
        x <- S.param' "id"
        -- d   <- (localDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
        tod <- (localTimeOfDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
        day <- liftIO $ DBQ.getDayFromID x
        todos <- liftIO DBQ.getTodos
        case day of
            Just d -> R.day x tod d  Nothing todos
            Nothing -> R.err "No day with that ID"


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
        id <- S.param' "todoid"
        dayid <- S.param' "dayid"
        _ <- liftIO $ DBU.removeTodo conn id
        S.redirect $ makeQueryString "/day" ("id", show (dayid :: Int))
