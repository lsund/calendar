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
            h2_ "New note"
            form_ [method_ "post"] $ do
                label_ $ do
                    "Author:"
                    input_ [name_ "author"]
                label_ $ do
                    "Contents: "
                    textarea_ [name_ "contents"] ""
                input_
                    [type_ "submit", value_ "Add note"]

rootPOST :: Server ()
rootPOST =
    post root $ do
        a <- param' "author"
        -- c <- param' "contents"
        entriesRef <- entries <$> getState
        liftIO $ atomicModifyIORef' entriesRef $ \es ->
            (es <> [Entry (Time 8 0) a True], ())
        redirect "/"


