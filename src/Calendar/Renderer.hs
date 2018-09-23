module Calendar.Renderer where

import           Data.List               (head, (!!))
import           Data.Time.LocalTime
import           Lucid
import           Protolude               hiding (head)
import           Web.Spock               hiding (head)
import           Web.Spock.Lucid         (lucid)

import           Calendar.Data.Day       (Day (..))
import qualified Calendar.Data.Day       as Day
import qualified Calendar.Data.Todo      as TODO
import           Calendar.Forecast
import qualified Calendar.ViewComponents as VC

type TODO = TODO.TODO
type Date = Day.Date

pairForecast :: [Day] -> [Weather] -> [(Day, Maybe Weather)]
pairForecast days fc = map f days
    where
        f d@(Day _ date _) =
            case find (\w -> localDay (_time w) == date) fc of
                    Just w  -> (d, Just w)
                    Nothing -> (d, Nothing)


monthNavbar :: Date -> HtmlT Identity ()
monthNavbar date =
    VC.navbar
        date
        "/month"
        "/week"
        "Week"
        ""
        "Day"
        (Day.subMonth date)
        (Day.addMonth date)


weekNavbar :: Date -> HtmlT Identity ()
weekNavbar date =
    VC.navbar
        date
        "/week"
        "/month"
        "Month"
        ""
        "Day"
        (enumFromThen date (pred date) !! 7)
        (enumFrom date !! 7)


dayNavbar :: Date -> HtmlT Identity ()
dayNavbar date =
    VC.navbar
        date
        ""
        "/week"
        "Week"
        "/month"
        "Month"
        (pred date)
        (succ date)
layout :: MonadIO m => HtmlT Identity a -> ActionCtxT cxt m b
layout b =
    lucid $ do
        head_ $ do
            link_ [rel_ "stylesheet", href_ "/styles.css"]
            link_ [rel_ "stylesheet", href_ "/mui.css"]
            title_ "Calendar"
        body_ $ div_ [class_ "content"] b


dayLayout :: MonadIO m => Day -> TimeOfDay -> Maybe Weather -> [TODO] -> ActionCtxT cxt m b
dayLayout day tod w todos =
    layout $
        div_ [class_ "mui-container"] $ do
            dayNavbar (Day._date day)
            VC.day day w tod
            div_ (VC.todo day todos)


weekLayout :: MonadIO m => Int -> Day.Date -> [Day] -> ActionCtxT ctx m b
weekLayout wn date days =
    layout $
        div_ [class_ "mui-container"] $ do
            weekNavbar date
            div_ (h3_ $ toHtml ("Week " <> show wn :: Text))
            ul_ $ forM_ (zip [1..] days) (\(i, d) -> (li_ $ VC.weekDay (i, d)))


monthLayout :: MonadIO m => Day.Date -> [Day] -> ActionCtxT ctx m b
monthLayout date days =
    layout $
        div_ [class_ "mui-container"] $ do
            monthNavbar date
            ul_ $ forM_ (zip dayCycle days) (\(i, d) -> (li_ $ VC.weekDay (i, d)))
        where
            firstWeekDay = pred (Day.dayOfWeek $ (_date . head) days)
            dayCycle = drop firstWeekDay (cycle [1..7])


err :: MonadIO m => Text -> ActionCtxT cxt m b
err msg = layout $ do
            h1_ $ toHtml ("Error" :: Text)
            div_ $ toHtml msg

