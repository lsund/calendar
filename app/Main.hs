{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Lib

import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.Text              (Text)
import           Data.Time.LocalTime
import           Lucid
import           Protolude              hiding (get)
import           Web.Spock
import           Web.Spock.Config
import           Web.Spock.Lucid        (lucid)

data Note = Note { author :: Text, contents :: Text }

newtype ServerState = ServerState { notes :: IORef [Note] }

type Server a = SpockM () () ServerState a

todayFile = "data/2018/04/16.txt"

app :: Server ()
app = do
    Right (Day x ys) <- liftIO $ parseFile todayFile
    (ZonedTime lt tz) <- liftIO getZonedTime
    print $ toTime lt
    get root $ do
        notes' <- getState >>= (liftIO . readIORef . notes)
        lucid $ do
            h1_ "Planner"
            ul_ $ forM_ ys $ \e -> li_ [class_ "test"] $
                toHtml (show e :: Text)
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
    post root $ do
        author <- param' "author"
        contents <- param' "contents"
        notesRef <- notes <$> getState
        liftIO $ atomicModifyIORef' notesRef $ \notes ->
            (notes <> [Note author contents], ())
        liftIO $ appendFile "data/test.txt" author
        redirect "/"


main :: IO ()
main = do
    st <- ServerState <$>
            newIORef
            [ Note "Alice" "Must not forget to walk the dog"
            , Note "Bob" "Must eat pizza" ]
    cfg <- defaultSpockCfg () PCNoDatabase st
    runSpock 8080 (spock cfg app)
