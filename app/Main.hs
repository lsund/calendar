{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Calendar.Handler

import           Network.Wai.Middleware.Static
import           Protolude
import           Web.Spock
import           Web.Spock.Config


app :: Server ()
app = do
    middleware $ staticPolicy $ addBase "static"
    rootGET
    addPOST
    updatePOST
    donePOST


main :: IO ()
main = do
    cfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 (spock cfg app)
