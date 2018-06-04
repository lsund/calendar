{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Network.Wai.Middleware.Static
import           Protolude
import qualified Web.Spock                     as S
import           Web.Spock.Config

import           Calendar.Database.Internal
import           Calendar.Handler


app :: Server ()
app = do
    S.middleware $ staticPolicy $ addBase "static"
    conn <- liftIO makeConnection
    getRootWeather conn
    getRoot conn
    add conn
    update conn
    done conn
    delete conn


main :: IO ()
main = do
    cfg <- defaultSpockCfg () PCNoDatabase ()
    S.runSpock 8080 (S.spock cfg app)
