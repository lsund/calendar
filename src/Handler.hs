module Handler where

import           Control.Monad.IO.Class (liftIO)
import           Data.Time.LocalTime
import           Lucid
import           Protolude              hiding (get)
import           Web.Spock
import           Web.Spock.Lucid        (lucid)

import qualified Components             as C
import           Date
import           DateTime
import           Day
import           Parser
import           Time
import qualified Writer                 as W


type Server a = SpockM () () () a

nfiles :: Int
nfiles = 3


renderIndex :: (MonadIO m) => Time -> [Day] -> ActionCtxT cxt m b
renderIndex t days =
    lucid $ do
        link_ [rel_ "stylesheet", href_ "styles.css"]

        h1_ $ toHtml (show t :: Text)
        forM_ days $ \day ->
            C.day day t
        C.newEntry


rootGET :: Server ()
rootGET =
    get root $ do

        (ZonedTime lt _) <- liftIO getZonedTime

        let (DateTime d t) = toDateTime lt
            ds             = iterate succDate d
            fs             = take nfiles $ map dateToPath ds

        results  <- mapM (liftIO . parseFile) (zip ds fs)

        if any isLeft results
            then print ("Could not parse a file" :: Text)
            else renderIndex t (rights results)


rootPOST :: Server ()
rootPOST =
    post root $ do
        h                <- param' "hour"
        m                <- param' "minute"
        desc             <- param' "desc"
        (ZonedTime lt _) <- liftIO getZonedTime

        -- This is always writing to todays file, since lt is today. Should
        -- change in the future
        liftIO $ W.writeToFile (toDate lt) h m desc

        redirect "/"


donePOST :: Server ()
donePOST =
    post "done" $
        do
        i <- param' "id"
        print (i :: Text)
        redirect "/"
