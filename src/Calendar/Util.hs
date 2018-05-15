module Calendar.Util where

import Protolude

apply3 :: (a -> b) -> (a -> c) -> (a -> d) -> [a] -> [(b, c, d)]
apply3 _ _ _  [] = []
apply3 f g h (x : xs) = (f x, g x, h x) : apply3 f g h xs


sequence3 :: Monad m => (m a, m b, m c) -> m (a, b, c)
sequence3 (ma, mb, mc) = (,,) <$> ma <*> mb <*> mc


