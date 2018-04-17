module Parser where

import           Data.Functor.Identity
import           Data.List                     (unwords)
import           GHC.Show
import           Prelude                       (String, read)
import           Protolude                     hiding (Show, show, (<|>))
import           Text.Parsec
import           Text.ParserCombinators.Parsec

import           Time

data Date = Date
    { _year  :: Int
    , _month :: Int
    , _day   :: Int
    } deriving (Show)

data Entry = Entry { _time :: Time, _desc :: String, _done :: Bool }

instance Show Entry where
    show (Entry t desc _) = show t ++ " " ++ desc

data Day = Day Date [Entry] deriving (Show)

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
    _ <- newline
    return $ Date (read y) (read m) (read d)

time :: DayParser () Time
time = do
    h <- count 2 digit
    m <- count 2 digit
    return $ Time (read h) (read m)


word :: DayParser () String
word = many1 (noneOf "\n")


done :: DayParser () Bool
done = do
    c <- char 'D' <|> char 'T'
    space
    return $ c == 'D'


entry :: DayParser () Entry
entry = do
    d <- done
    t <- time
    _ <- space
    ss <- word `sepBy` char ' '
    return $ Entry t (unwords ss) d

content :: DayParser () Day
content = do
    d <- date
    _ <- newline
    es <- entry `endBy` newline <* eof
    return $ Day d es

parseFile :: String -> IO (Either ParseError Day)
parseFile = parseFromFile content
