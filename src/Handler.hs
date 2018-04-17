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


type Server a = SpockM () () ServerState a
newtype ServerState = ServerState { entries :: IORef [Entry] }


rootGET :: Server ()
rootGET =
    get root $ do
        es    <- getState >>= (liftIO . readIORef . entries)
        (ZonedTime lt _) <- liftIO getZonedTime
        let ct = toTime lt
        lucid $ do
            link_ [rel_ "stylesheet", href_ "styles.css"]
            h1_ $ toHtml (show ct :: Text)
            entryList es ct
            entryForm


rootPOST :: Server ()
rootPOST =
    post root $ do
        h <- param' "hour"
        m <- param' "minute"
        desc <- param' "desc"
        entriesRef <- entries <$> getState
        liftIO $ atomicModifyIORef' entriesRef $ \es ->
            (es <> [Entry (Time h m) desc False], ())
        redirect "/"
