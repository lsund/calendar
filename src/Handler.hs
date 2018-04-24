module Handler where

import           Control.Monad.IO.Class (liftIO)
import           Data.Time.LocalTime
import           Lucid
import           Protolude              hiding (get)
import           Web.Spock
import           Web.Spock.Lucid        (lucid)

import qualified Components             as C
import           DateTime
import           CalendarDay
import           Parser
import qualified Writer                 as W


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

        (ZonedTime lt _) <- liftIO getZonedTime
        let (DateTime d t) = toDateTime lt

        days <- liftIO $ readDays d nfiles

        if any isLeft days
            then print ("Could not parse a file" :: Text)
            else renderIndex t (rights days)


rootPOST :: Server ()
rootPOST =
    post root $ do
        -- h                <- param' "hour"
        -- m                <- param' "minute"
        -- desc             <- param' "desc"
        (ZonedTime lt _) <- liftIO getZonedTime

        let (DateTime d t) = toDateTime lt
        days <- liftIO $ readDays d 1

        -- This is always writing to todays file, since lt is today. Should
        -- change in the future
        if any isLeft days
            then return ()
            else
                let (x : _) = rights days
                in liftIO $ W.serialize x (Entry t "Blah" False)

        redirect "/"


donePOST :: Server ()
donePOST =
    post "done" $
        do
        i <- param' "id"
        print (i :: Text)
        redirect "/"
