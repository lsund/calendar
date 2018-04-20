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

newtype ServerState = ServerState { _days :: IORef [Day] }


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

                let parsedDays = rights results

                lucid $ do
                    link_ [rel_ "stylesheet", href_ "styles.css"]
                    h1_ $ toHtml (show t :: Text)
                    forM_ parsedDays $ \day ->
                        C.day day t
                    C.entryForm

line :: Date -> Int -> Int -> Text -> Text
line d h m desc = "T " <> show (Time h m) <> " " <> desc <> "\n"

rootPOST :: Server ()
rootPOST =
    post root $
        do
        h <- param' "hour"
        m <- param' "minute"
        desc <- param' "desc"
        (ZonedTime lt _) <- liftIO getZonedTime

        let t = toTime lt
            d = toDate lt
        liftIO $ appendFile (dateToPath d) (line d h m desc)
        redirect "/"
