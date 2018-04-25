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
            forM_ days $ \day ->
                C.day day t
            C.newEntry


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
    post root $
        -- h                <- param' "hour"
        -- m                <- param' "minute"
        -- desc             <- param' "desc"
        -- (ZonedTime lt _) <- liftIO getZonedTime
        -- let (DateTime d t) = toDateTime lt

        -- let ds = take 10 $ iterate succ d

        -- days <- liftIO $ mapM getDay ds

        -- This is always writing to todays file, since lt is today. Should
        -- change in the future
        -- if any isLeft days
        --     then return ()
        --     else
        --         let (x : _) = rights days
        --         in liftIO $ W.serialize x (Entry t "Blah" False)

        redirect "/"


donePOST :: Server ()
donePOST =
    post "done" $ do
        t <- param' "time"
        d <- param' "day"

        _ <- liftIO $ updateDoneEntry d t

        redirect "/"
