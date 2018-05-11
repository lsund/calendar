module Calendar.Util where

apply3 :: (a -> b) -> (a -> c) -> (a -> d) -> [a] -> [(b, c, d)]
apply3 _ _ _  [] = []
apply3 f g h (x : xs) = (f x, g x, h x) : apply3 f g h xs

