{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Network.Wai.Middleware.Static
import           Protolude
import           Web.Spock
import           Web.Spock.Config

import           Calendar.Handler
import           Calendar.Database.Internal


app :: Server ()
app = do
    middleware $ staticPolicy $ addBase "static"
    conn <- liftIO makeConnection
    rootGET conn
    addPOST conn
    updatePOST conn
    donePOST conn


main :: IO ()
main = do
    cfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 (spock cfg app)
