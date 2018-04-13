module Lib where

import           Data.Functor.Identity
import           Text.Parsec
import           Text.ParserCombinators.Parsec

data Date = Date
    { _year  :: Int
    , _month :: Int
    , _day   :: Int
    } deriving (Show)

data Time = Time
    { _hour :: Int
    , _minute :: Int
    } deriving (Show)


data Day = Day Date [Time] deriving (Show)

type DayParser u = ParsecT String u Identity

filePath :: FilePath
filePath = "data/data.txt"

run :: IO ()
run = do
    x <- parseFile filePath
    case x of
        Left e     -> print e
        Right cont -> print cont

date :: DayParser () Date
date = do
    y <- count 4 digit
    _ <- char '-'
    m <- count 2 digit
    _ <- char '-'
    d <- count 2 digit
    return $ Date (read y) (read m) (read d)

time :: DayParser () Time
time = do
    h <- count 2 digit
    m <- count 2 digit
    return $ Time (read h) (read m)

content :: DayParser () Day
content = do
    d <- date
    _ <- newline
    _ <- newline
    t <- time
    return $ Day d [t]


parseFile :: String -> IO (Either ParseError Day)
parseFile = parseFromFile content
