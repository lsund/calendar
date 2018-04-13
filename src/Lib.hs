module Lib where

run :: IO ()
run = do
    cont <- readFile "data/data.txt"
    putStrLn cont
