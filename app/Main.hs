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
    week conn
    day conn
    entryAdd conn
    entryUpdate conn
    entryDone conn
    entryDelete conn
    entryPush conn
    todoAdd conn
    todoRemove conn
    todoUpdate conn
    browseDate conn


main :: IO ()
main = do
    let port = 3002
    cfg <- defaultSpockCfg () PCNoDatabase ()
    S.runSpock port (S.spock cfg app)
    putStrLn $ "Running on port: " ++ show port
