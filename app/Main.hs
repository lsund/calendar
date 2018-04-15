module Main where

-- import Lib

import Web.Spock
import Web.Spock.Config

type Server a = SpockM () () () a

app :: Server ()
app = get root (text "Hello!")

main :: IO ()
main = do
    cfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 (spock cfg app)

