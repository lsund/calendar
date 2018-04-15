{-# LANGUAGE NoImplicitPrelude #-}

module Main where

-- import Lib

import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.Text              (Text)
import           Lucid
import           Protolude hiding (get)
import           Web.Spock
import           Web.Spock.Config
import           Web.Spock.Lucid        (lucid)

data Note = Note { author :: Text, contents :: Text }

newtype ServerState = ServerState { notes :: IORef [Note] }

type Server a = SpockM () () ServerState a

app :: Server ()
app = do
    cont <- liftIO $ readFile "data/test.txt"
    get root $ do
        notes' <- getState >>= (liftIO . readIORef . notes)
        lucid $ do
            h1_ "Notes"
            p_ $ toHtml cont
            ul_ $ forM_ notes' $ \note -> li_ $ do
                toHtml (author note)
                ": "
                toHtml (contents note)
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

