module Parser where

import           Data.Functor.Identity
import           Data.Text                     (append, pack, unwords)
import           Prelude                       (String, read)
import           Protolude                     hiding ((<|>))
import           System.Directory
import           Text.Parsec
import           Text.ParserCombinators.Parsec

import           Date
import           Day
import           Time

type DayParser u = ParsecT String u Identity


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
    _ <- char ':'
    m <- count 2 digit
    return $ Time (read h) (read m)


word :: DayParser () String
word = many1 (noneOf "\n")


done :: DayParser () Bool
done = do
    c <- char 'D' <|> char 'T'
    _ <- space
    return $ c == 'D'


entry :: DayParser () Entry
entry = do
    d  <- done
    t  <- time
    _  <- space
    ss <- word `sepBy` char ' '
    return $ Entry t (unwords (map pack ss)) d

content :: DayParser () Day
content = do
    d <- date
    _ <- newline
    es <- entry `endBy` newline <* eof
    return $ Day d es


parseFile :: (Date, FilePath) -> IO (Either ParseError Day)
parseFile (d@(Date y m _), s) = do
        let path = dateToPath d

        createDirectoryIfMissing True (yearMonthPath y m)
        exist <- doesFileExist path
        when (not exist) $ writeFile path (show d `append` "\n\nT 01:00 TODO\n")

        parseFromFile content s


readDays :: Date -> Int -> IO [Either ParseError Day]
readDays d n = do
    let ds = iterate succDate d
        fs = take n $ map dateToPath ds
    mapM (liftIO . parseFile) (zip ds fs)
