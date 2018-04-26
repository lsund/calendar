module Handler where

import           Control.Monad.IO.Class (liftIO)
import           Data.Time.LocalTime
import           Lucid
-- import           Prelude                (read)
import           Protolude              hiding (get)
import           Web.Spock
import           Web.Spock.Lucid        (lucid)

import           CalendarDay
import qualified Components             as C
import           Database


type Server a = SpockM () () () a

nfiles :: Int
nfiles = 7


renderIndex :: (MonadIO m) => TimeOfDay -> [CalendarDay] -> ActionCtxT cxt m b
renderIndex t days =
    lucid $
        div_ [class_ "mui-container"] $ do
            link_ [rel_ "stylesheet", href_ "styles.css"]
            link_ [rel_ "stylesheet", href_ "mui.css"]

            h1_ $ toHtml (show t :: Text)
            C.newEntry
            forM_ days $ \day ->
                C.day day t


rootGET :: Server ()
rootGET =
    get root $ do

        d <- (localDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
        tod <- (localTimeOfDay . zonedTimeToLocalTime) <$> liftIO getZonedTime

        let dates = take 10 $ iterate succ d
        days <- liftIO $ mapM getDay dates

        renderIndex tod days


rootPOST :: Server ()
rootPOST =
    post root $ do
        h                <- param' "hour"
        m                <- param' "minute"
        desc             <- param' "desc"
        let e = Entry (TimeOfDay h m 0) desc False
        print e
        d <- (localDay . zonedTimeToLocalTime) <$> liftIO getZonedTime

        _ <- liftIO $ addEntry d e

        redirect "/"


donePOST :: Server ()
donePOST =
    post "done" $ do
        t <- param' "time"
        d <- param' "day"

        _ <- liftIO $ updateDoneEntry d t

        redirect "/"
