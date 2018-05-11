module Calendar.Handler where

import Prelude (String)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson
import qualified           Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import           Data.Text.Lazy             (pack)
import           Data.Text.Lazy.Encoding    (encodeUtf8)
import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple
import           Network.Curl
import           Protolude                  hiding (encodeUtf8)
import qualified Web.Spock                  as S

import           Calendar.Data.Entry
import           Calendar.Database.Query
import           Calendar.Database.Update
import qualified Calendar.Renderer          as R


type Server a = S.SpockM () () () a

nfiles :: Int
nfiles = 45

readJSON :: String -> Maybe (HashMap Text Value)
readJSON = decode . encodeUtf8 . pack

toObject :: Value -> Object
toObject (Object o) = o
toObject _ = M.empty

getRoot :: Connection -> Server ()
getRoot _ =
    S.get S.root $ do

        (_, resp) <- liftIO $ curlGetString "http://api.openweathermap.org/data/2.5/weather?q=Dusseldorf,de&APPID=0225725c608003e41c3e7936f6e6700b" []
        let (Just (Number x)) = readJSON resp >>= M.lookup "main" >>= M.lookup "temp" . toObject
            temp =  x - 273.15

        d   <- (localDay . zonedTimeToLocalTime) <$> liftIO getZonedTime
        tod <- (localTimeOfDay . zonedTimeToLocalTime) <$> liftIO getZonedTime

        let dates = take nfiles $ iterate succ d
        days <- liftIO $ mapM getDay dates
        R.index temp tod days


add :: Connection -> Server ()
add conn =
    S.post "add" $ do
        time <- S.param' "time"
        desc <- S.param' "desc"
        id   <- S.param' "id"
        let e = Entry 0 time desc False

        _ <- liftIO $ insertEntry conn id e
        S.redirect "/"


update :: Connection -> Server ()
update conn =
    S.post "update" $ do
        t    <- S.param' "time"
        desc <- S.param' "desc"
        id   <- S.param' "id"
        isdone <- S.param' "done"

        _ <- liftIO $ updateEntry conn id t desc isdone

        S.redirect "/"


done :: Connection -> Server ()
done conn =
    S.post "done" $ do
        id <- S.param' "id"
        _ <- liftIO $ entryDone conn id

        S.redirect "/"


delete :: Connection -> Server ()
delete conn =
    S.post "delete" $ do
        id <- S.param' "id"
        _ <- liftIO $ deleteEntry conn id

        S.redirect "/"
