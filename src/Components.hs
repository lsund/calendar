
module Components where

import           Control.Monad (forM_)
import           Lucid
import           Protolude

import           Day
import           Time


classList :: Entry -> Time -> [Attribute]
classList e ct
    | _done e             = [class_ "done"]
    | ct `isPast` _time e = [class_ "past"]
    | otherwise           = []


entry :: Entry -> HtmlT Identity ()
entry e
    | _done e = div_ [class_ "mui--divider-bottom"] (toHtml (show e :: Text))
    | otherwise =
        form_ [method_ "post", action_ "done?id=1", class_ "mui-form--inline"] $ do
            input_ [type_ "submit", value_ "done", class_ "mui-btn mui-btn--small"]
            div_ [class_ "mui-textfield"] $
                input_ [type_ "text", value_ (show e :: Text)]


day :: Day -> Time -> HtmlT Identity ()
day (Day date es) ct = do
    h2_ $ toHtml (show date :: Text)
    ul_ $ forM_ es (\e -> li_ (classList e ct) $ entry e)


newEntry :: HtmlT Identity ()
newEntry = do
    h2_ "New Entry"
    form_ [method_ "post"] $ do
        label_ $ do
            "Hour:"
            input_ [type_ "number", name_ "hour"]
        label_ $ do
            "Minute:"
            input_ [type_ "number", name_ "minute"]
        br_ []
        label_ $ do
            "Description: "
            textarea_ [name_ "desc"] ""
        br_ []
        input_
            [type_ "submit", value_ "Add Entry"]

