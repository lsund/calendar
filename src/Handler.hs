module Handler where

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
data Note = Note { author :: Text, contents :: Text }
newtype ServerState = ServerState { notes :: IORef [Note] }

classList :: Entry -> Time -> [Attribute]
classList e ct
    | _done e             = [class_ "done"]
    | ct `isPast` _time e = [class_ "past"]
    | otherwise           = []


rootGET :: [Entry] -> Server ()
rootGET ys = do
    (ZonedTime lt tz) <- liftIO getZonedTime
    let ct = toTime lt
    get root $ do
        notes' <- getState >>= (liftIO . readIORef . notes)
        lucid $ do
            link_ [rel_ "stylesheet", href_ "styles.css"]
            h1_ "Planner"
            ul_ $ forM_ ys $ \e ->
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
        author <- param' "author"
        contents <- param' "contents"
        notesRef <- notes <$> getState
        liftIO $ atomicModifyIORef' notesRef $ \notes ->
            (notes <> [Note author contents], ())
        liftIO $ appendFile "data/test.txt" author
        redirect "/"


