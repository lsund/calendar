module Calendar.Util where

import Protolude
import qualified Data.Text                  as T
import Data.Time

apply3 :: (a -> b) -> (a -> c) -> (a -> d) -> [a] -> [(b, c, d)]
apply3 _ _ _  [] = []
apply3 f g h (x : xs) = (f x, g x, h x) : apply3 f g h xs


sequence3 :: Monad m => (m a, m b, m c) -> m (a, b, c)
sequence3 (ma, mb, mc) = (,,) <$> ma <*> mb <*> mc


parseMaybeTime :: Text -> Maybe TimeOfDay
parseMaybeTime x =
    let parsed = parseTimeM True defaultTimeLocale "%H:%M" (T.unpack x)
    in parsed


makeQueryString :: Text -> (Text, Text) -> Text
makeQueryString url (param, value) = url <> "?" <> param <> "=" <> value
