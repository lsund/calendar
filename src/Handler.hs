module Handler where

import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.Time.LocalTime
import           Lucid
import           Protolude              hiding (get)
import           Web.Spock
import           Web.Spock.Lucid        (lucid)

import           Components
import           Date
import           Day
import           Parser
import           Time


type Server a = SpockM () () ServerState a

newtype ServerState = ServerState { entries :: IORef [Day] }


nfiles = 3 :: Int


rootGET :: Server ()
rootGET =
    get root $ do

        (ZonedTime lt _) <- liftIO getZonedTime

        let t = toTime lt
            d = toDate lt

            dates = iterate succDate d
            files = take 3 $ map dateToPath dates

        results  <- mapM (liftIO . parseFile) (zip dates files)

        if any isLeft results
            then return ()
            else do
                let days = rights results
                entriesRef <- entries <$> getState
                liftIO $ atomicModifyIORef' entriesRef $ const (days, ())
                lucid $ do
                    link_ [rel_ "stylesheet", href_ "styles.css"]
                    h1_ $ toHtml (show t :: Text)
                    forM_ days $ \d ->
                        day d t
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
