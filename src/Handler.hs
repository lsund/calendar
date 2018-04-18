module Handler where

import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.Time.LocalTime
import           Lucid
import           Protolude              hiding (get)
import           Web.Spock
import           Web.Spock.Lucid        (lucid)

import           Components
import           Day
import           Parser
import           Time


type Server a = SpockM () () ServerState a
newtype ServerState = ServerState { entries :: IORef ([Entry], [Entry]) }

rootGET :: Server ()
rootGET =
    get root $ do
        (ZonedTime lt _) <- liftIO getZonedTime
        let time = toTime lt
            date = toDate lt
            todayFile = dateToPath date
            tomorrowFile = dateToPath (succDate date)
        Right (Day _ today) <- liftIO $ parseFile todayFile
        Right (Day _ tomorrow) <- liftIO $ parseFile tomorrowFile
        entriesRef <- entries <$> getState
        liftIO $ atomicModifyIORef' entriesRef $ const ((today, tomorrow), ())
        lucid $ do
            link_ [rel_ "stylesheet", href_ "styles.css"]
            h1_ $ toHtml (show time :: Text)
            entryList today date time
            entryList tomorrow (succDate date) time
            entryForm


rootPOST :: Server ()
rootPOST =
    post root $ do
        h <- param' "hour"
        m <- param' "minute"
        desc <- param' "desc"
        entriesRef <- entries <$> getState
        liftIO $ atomicModifyIORef' entriesRef $ \(es, es') ->
            ((es <> [Entry (Time h m) desc False], es'), ())
        redirect "/"
