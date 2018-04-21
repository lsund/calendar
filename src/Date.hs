module Date where

import           GHC.Base
import           GHC.Show            (Show, show)
import           Protolude           hiding (show)

import           Data.Time.Calendar
import           Data.Time.LocalTime

data Date = Date
    { _year  :: Integer
    , _month :: Int
    , _day   :: Int
    }


instance Show Date where
    show (Date y m d) = show y <> "-" <> padd m <> "-" <> padd d


padd :: (Show a, Num a, Ord a) => a -> GHC.Base.String
padd x
    | x < 10    = "0" <> GHC.Show.show x
    | otherwise = GHC.Show.show x


toDate :: LocalTime -> Date
toDate (LocalTime day _) = let (y, m, d) = toGregorian day in Date y m d


-- Cannot implement enum for dates, since `_year` is Integer, not Int
succDate :: Date -> Date
succDate (Date y m d) = Date y m $ succ d


yearMonthPath :: Integer -> Int -> FilePath
yearMonthPath y m = concat ["data/", show y, "/", padd m]


dateToPath :: Date -> FilePath
dateToPath (Date y m d) = concat [yearMonthPath y m, "/", padd d, ".txt"]
