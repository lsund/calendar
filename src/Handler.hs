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
newtype ServerState = ServerState { entries :: IORef [Day] }

rootGET :: Server ()
rootGET =
    get root $ do
        (ZonedTime lt _) <- liftIO getZonedTime
        let time = toTime lt
            date = toDate lt

            todayFile = dateToPath date
            tomorrowFile = dateToPath (succDate date)
            threedayFile = dateToPath (succDate (succDate date))

            files = [todayFile, tomorrowFile, threedayFile]
        results  <- mapM (liftIO . parseFile) files
        if any isLeft results
            then return ()
            else
                do
                    let days = rights results
                    entriesRef <- entries <$> getState
                    liftIO $ atomicModifyIORef' entriesRef $ const (days, ())
                    lucid $ do
                        link_ [rel_ "stylesheet", href_ "styles.css"]
                        h1_ $ toHtml (show time :: Text)
                        forM_ days $ \(Day _ entries) ->
                            entryList entries date time
                        entryForm


rootPOST :: Server ()
rootPOST =
    post root $
        -- do
        -- h <- param' "hour"
        -- m <- param' "minute"
        -- desc <- param' "desc"
        -- entriesRef <- entries <$> getState
        -- liftIO $ atomicModifyIORef' entriesRef $ \(es, es') ->
        --     ((es <> [Entry (Time h m) desc False], es'), ())
        redirect "/"
