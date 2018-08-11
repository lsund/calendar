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
    day conn
    entryAdd conn
    entryUpdate conn
    entryDone conn
    entryDelete conn
    entryPush conn
    todoAdd conn
    todoRemove conn
    todoUpdate conn


main :: IO ()
main = do
    cfg <- defaultSpockCfg () PCNoDatabase ()
    S.runSpock 8080 (S.spock cfg app)
