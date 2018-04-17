{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Handler
import           Lib

import           Control.Monad.IO.Class        (liftIO)
import           Data.IORef
import           Network.Wai.Middleware.Static
import           Protolude
import           Web.Spock
import           Web.Spock.Config

-- To be removed
-- data Note = Note { author :: Text, contents :: Text }
-- newtype ServerState = ServerState { notes :: IORef [Note] }

todayFile :: FilePath
todayFile = "data/2018/04/17.txt"

app :: Server ()
app = do
    Right (Day x ys) <- liftIO $ parseFile todayFile
    middleware $ staticPolicy $ addBase "static"
    rootGET ys
    rootPOST


main :: IO ()
main = do
    st <- ServerState <$>
            newIORef
            [ Note "Alice" "Must not forget to walk the dog"
            , Note "Bob" "Must eat pizza" ]
    cfg <- defaultSpockCfg () PCNoDatabase st
    runSpock 8080 (spock cfg app)
