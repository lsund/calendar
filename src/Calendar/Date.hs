module Calendar.Date where

import           Data.Time.Calendar
import           GHC.Base
import           GHC.Show           (Show, show)
import           Protolude          hiding (show)

padd :: (Show a, Num a, Ord a) => a -> GHC.Base.String
padd x
    | x < 10    = "0" <> GHC.Show.show x
    | otherwise = GHC.Show.show x


yearMonthPath :: Integer -> Int -> FilePath
yearMonthPath y m = concat ["data/", show y, "/", padd m]


dateToPath :: Day -> FilePath
dateToPath day = concat [yearMonthPath y m, "/", padd d, ".txt"]
    where (y, m, d) = toGregorian day
