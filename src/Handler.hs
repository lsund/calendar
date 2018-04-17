module Handler where

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad   (forM_)
import           Data.IORef
import           Lucid
import           Protolude       hiding (get)
import           Web.Spock
import           Web.Spock.Lucid (lucid)
import           Data.Time.LocalTime

import           Lib
import           Time


type Server a = SpockM () () ServerState a
newtype ServerState = ServerState { entries :: IORef [Entry] }


todayFile :: FilePath
todayFile = "data/2018/04/17.txt"


classList :: Entry -> Time -> [Attribute]
classList e ct
    | _done e             = [class_ "done"]
    | ct `isPast` _time e = [class_ "past"]
    | otherwise           = []


rootGET :: Server ()
rootGET =
    get root $ do
        es    <- getState >>= (liftIO . readIORef . entries)
        (ZonedTime lt _) <- liftIO getZonedTime
        let ct = toTime lt
        lucid $ do
            link_ [rel_ "stylesheet", href_ "styles.css"]
            h1_ $ toHtml (show ct :: Text)
            ul_ $ forM_ es $ \e ->
                let cs = classList e ct
                in li_ cs $ toHtml (show e :: Text)
            h2_ "New Entry"
            form_ [method_ "post"] $ do
                label_ $ do
                    "Hour:"
                    input_ [type_ "number", name_ "hour"]
                label_ $ do
                    "Minute:"
                    input_ [type_ "number", name_ "minute"]
                br_ []
                label_ $ do
                    "Description: "
                    textarea_ [name_ "desc"] ""
                br_ []
                input_
                    [type_ "submit", value_ "Add Entry"]

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


