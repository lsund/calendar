module Handler where

import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.Time.LocalTime
import           Lucid
import           Protolude              hiding (get)
import           Web.Spock
import           Web.Spock.Lucid        (lucid)

import           Components
import           Parser
import           Time
import           Day


type Server a = SpockM () () ServerState a
newtype ServerState = ServerState { entries :: IORef ([Entry], [Entry]) }

todayFile :: FilePath
todayFile = "data/2018/04/18.txt"

tomorrowFile :: FilePath
tomorrowFile = "data/2018/04/19.txt"


rootGET :: Server ()
rootGET =
    get root $ do
        Right (Day _ today) <- liftIO $ parseFile todayFile
        Right (Day _ tomorrow) <- liftIO $ parseFile tomorrowFile
        entriesRef <- entries <$> getState
        liftIO $ atomicModifyIORef' entriesRef $ const ((today, tomorrow), ())
        (ZonedTime lt _) <- liftIO getZonedTime
        let ct = toTime lt
        lucid $ do
            link_ [rel_ "stylesheet", href_ "styles.css"]
            h1_ $ toHtml (show ct :: Text)
            entryList today ct
            entryList tomorrow ct
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
