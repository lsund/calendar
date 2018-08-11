module Calendar.Handler where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Time
import           Data.Time.Calendar  as T
-- import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple
import           Protolude
import           Web.Spock                  ((<//>), var)
import qualified Web.Spock                  as S

import           Calendar.Data.Day          as Day
import           Calendar.Data.Entry
import qualified Calendar.Database.Query    as DBQ
import qualified Calendar.Database.Update   as DBU
-- import           Calendar.Forecast
import qualified Calendar.Renderer          as R
import           Calendar.Util

type Server a = S.SpockM () () () a
type SpockState = S.WebStateM () () ()
type SpockContext a = S.ActionCtxT () SpockState a


day :: Connection -> Server ()
day _ =
    S.get (var <//> var <//> var) $ \year month day -> do
        let calenderDay = T.fromGregorian year month day
        tod <- (localTimeOfDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
        day <- liftIO $ DBQ.getDay calenderDay
        todos <- liftIO DBQ.getTodos
        R.day day tod Nothing todos


entryAdd :: Connection -> Server ()
entryAdd conn =
    S.post (var <//> var <//> var <//> "entry-add") $ \year month day -> do
        desc <- S.param' "desc"
        dayid   <- S.param' "dayid"
        let e = Entry 0 Nothing desc False
        _ <- liftIO $ DBU.insertEntry conn dayid e
        S.redirect $ Day.makeURL year month day


entryUpdate :: Connection -> Server ()
entryUpdate conn =
    S.post (var <//> var <//> var <//> "entry-update") $ \year month day -> do
        t    <- S.param' "time"
        desc <- S.param' "desc"
        entryid <- S.param' "entryid"
        isdone <- S.param' "done"
        let mt = parseMaybeTime t
        print desc
        _ <- liftIO $ DBU.updateEntry conn entryid mt desc isdone
        S.redirect $ Day.makeURL year month day


entryDone :: Connection -> Server ()
entryDone conn =
    S.post (var <//> var <//> var <//> "entry-done") $ \year month day -> do
        entryid <- S.param' "entryid"
        _ <- liftIO $ DBU.entryDone conn entryid
        S.redirect $ Day.makeURL year month day


entryDelete :: Connection -> Server ()
entryDelete conn =
    S.post (var <//> var <//> var <//> "entry-delete") $ \year month day -> do
        entryid <- S.param' "entryid"
        _ <- liftIO $ DBU.deleteEntry conn entryid
        S.redirect $ Day.makeURL year month day


entryPush :: Connection -> Server ()
entryPush conn =
    S.post (var <//> var <//> var <//> "entry-push") $ \year month day -> do
        entryid <- S.param' "entryid"
        _ <- liftIO $ DBU.push conn entryid
        S.redirect $ Day.makeURL year month day


todoAdd :: Connection -> Server ()
todoAdd conn =
    S.post (var <//> var <//> var <//> "todo-add") $ \year month day -> do
        t <- S.param' "desc"
        _ <- liftIO $ DBU.insertTodo conn t
        S.redirect $ Day.makeURL year month day


todoRemove :: Connection -> Server ()
todoRemove conn =
    S.post (var <//> var <//> var <//> "todo-remove") $ \year month day -> do
        todoid <- S.param' "todoid"
        _ <- liftIO $ DBU.removeTodo conn todoid
        S.redirect $ Day.makeURL year month day


todoUpdate :: Connection -> Server ()
todoUpdate conn =
    S.post (var <//> var <//> var <//> "todo-update") $ \year month day -> do
        todoid <- S.param' "todoid"
        desc <- S.param' "desc"
        _ <- liftIO $ DBU.updateTodo conn todoid desc
        S.redirect $ Day.makeURL year month day
