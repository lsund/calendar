{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Handler

import           Data.IORef
import           Lib
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
    middleware $ staticPolicy $ addBase "static"
    rootGET
    rootPOST


main :: IO ()
main = do
    Right (Day _ ys) <- liftIO $ parseFile todayFile
    st <- ServerState <$> newIORef ys
    cfg <- defaultSpockCfg () PCNoDatabase st
    runSpock 8080 (spock cfg app)
