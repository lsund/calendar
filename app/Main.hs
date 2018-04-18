{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Handler

import           Data.IORef
import           Network.Wai.Middleware.Static
import           Protolude
import           Web.Spock
import           Web.Spock.Config

-- To be removed
-- data Note = Note { author :: Text, contents :: Text }
-- newtype ServerState = ServerState { notes :: IORef [Note] }


app :: Server ()
app = do
    middleware $ staticPolicy $ addBase "static"
    rootGET
    rootPOST


main :: IO ()
main = do
    st <- ServerState <$> newIORef ([], [])
    cfg <- defaultSpockCfg () PCNoDatabase st
    runSpock 8080 (spock cfg app)
