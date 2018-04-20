module Handler where

import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.Time.LocalTime
import           Lucid
import           Protolude              hiding (get)
import           Web.Spock
import           Web.Spock.Lucid        (lucid)

import qualified Components as C
import           Date
import           Day
import           Parser
import           Time


type Server a = SpockM () () ServerState a

newtype ServerState = ServerState { entries :: IORef [Day] }


nfiles :: Int
nfiles = 3

rootGET :: Server ()
rootGET =
    get root $ do

        (ZonedTime lt _) <- liftIO getZonedTime

        let t = toTime lt
            d = toDate lt

            ds = iterate succDate d
            fs = take nfiles $ map dateToPath ds

        results  <- mapM (liftIO . parseFile) (zip ds fs)


        if any isLeft results
            then print ("Could not parse a file" :: Text)
            else do
                let days = rights results
                entriesRef <- entries <$> getState
                liftIO $ atomicModifyIORef' entriesRef $ const (days, ())
                lucid $ do
                    link_ [rel_ "stylesheet", href_ "styles.css"]
                    h1_ $ toHtml (show t :: Text)
                    forM_ days $ \day ->
                        C.day day t
                    C.entryForm


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
