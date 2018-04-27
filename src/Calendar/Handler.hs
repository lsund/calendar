module Calendar.Handler where

import           Control.Monad.IO.Class (liftIO)
import           Data.Time.LocalTime
-- import Data.Time.Calendar
import           Lucid
-- import           Prelude                (read)
import           Protolude              hiding (get)
import           Web.Spock
import           Web.Spock.Lucid        (lucid)

import           Calendar.Day
import qualified Calendar.Components    as C
import           Calendar.Database.Internal


type Server a = SpockM () () () a

nfiles :: Int
nfiles = 7


renderIndex :: (MonadIO m) => TimeOfDay -> [Day] -> ActionCtxT cxt m b
renderIndex t days =
    lucid $
        div_ [class_ "mui-container"] $ do
            link_ [rel_ "stylesheet", href_ "styles.css"]
            link_ [rel_ "stylesheet", href_ "mui.css"]

            h1_ $ toHtml (timeFormat t)
            forM_ days $ \day -> C.day day t


rootGET :: Server ()
rootGET =
    get root $ do

        d <- (localDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
        tod <- (localTimeOfDay . zonedTimeToLocalTime) <$> liftIO getZonedTime

        let dates = take 4 $ iterate succ d
        days <- liftIO $ mapM getDay dates
        renderIndex tod days



addPOST :: Server ()
addPOST =
    post "add" $ do
        h                <- param' "hour"
        m                <- param' "minute"
        desc             <- param' "desc"
        id                <- param' "id"
        let e = Entry 0 (TimeOfDay h m 0) desc False

        _ <- liftIO $ addEntry id e
        redirect "/"


updatePOST :: Server ()
updatePOST =
    post "update" $ do
        t <- param' "time"
        -- d <- param' "day"
        desc <- param' "desc"
        id <- param' "id"
        done <- param' "done"


        _ <- liftIO $ updateEntry id t desc done

        redirect "/"


donePOST :: Server ()
donePOST =
    post "done" $ do
        id <- param' "id"

        _ <- liftIO $ entryDone id

        redirect "/"
