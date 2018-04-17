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

rootGET ys = do
    (ZonedTime lt tz) <- liftIO getZonedTime
    let ct = toTime lt
    get root $ do
        notes' <- getState >>= (liftIO . readIORef . notes)
        lucid $ do
            link_ [rel_ "stylesheet", href_ "styles.css"]
            h1_ "Planner"
            ul_ $ forM_ ys $ \e ->
                let cont = toHtml (show e :: Text)
                in if ct `isPast` _time e then li_ [class_ "past"] cont
                -- in if _done e then li_ [class_ "done"] cont
                   else li_ cont
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

rootPOST =
    post root $ do
        author <- param' "author"
        contents <- param' "contents"
        notesRef <- notes <$> getState
        liftIO $ atomicModifyIORef' notesRef $ \notes ->
            (notes <> [Note author contents], ())
        liftIO $ appendFile "data/test.txt" author
        redirect "/"


