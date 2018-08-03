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
    getWeather conn
    getRoot conn
    getDay conn
    add conn
    update conn
    done conn
    delete conn
    push conn
    addTodo conn
    removeTodo conn


main :: IO ()
main = do
    cfg <- defaultSpockCfg () PCNoDatabase ()
    S.runSpock 8080 (S.spock cfg app)
