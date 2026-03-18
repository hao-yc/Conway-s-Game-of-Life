
{-# LANGUAGE OverloadedStrings #-}

module Develop.DatFw.Widget
    (-- * Widget types
      WidgetFor, Widget
    -- * Conversion
    , ToWidget(..), setTitle
    , Html
    -- * Page construction from Widget
    , PageContent(..), widgetToPageContent
    )
where
import Develop.DatFw.Internal.Types
import Develop.DatFw.Handler

import           Data.Monoid
import           Data.IORef
import           Data.Text as T
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Text.Blaze.Html

------------------- Widget conversion ------------------------

class ToWidget site a where
    toWidget :: a -> Widget site

{---
public static interface ToWidgetHead<site,a> {
    public abstract Widget<site> toWidgetHead(a x);
}

public static interface ToWidgetBody<site,a> {
    public abstract Widget<site> toWidgetBody(a x);
}
---}


setTitle :: Html -> Widget site
setTitle html = tellWidgetData mempty{ wdTitle = Just html }


------------------- ToWidget instances ----------------------------

-- instances of 'WidgetFor site ()'

instance (site' ~ site, a ~ ()) => ToWidget site (WidgetFor site' a) where
    toWidget = id

-- instances of '(Route site -> [(Text, Text)] -> Text) -> Html'

instance urender ~ UrlRender (Route site) => ToWidget site (urender -> Html) where
    toWidget = tellWidgetBody

{---
--  instance ToWidgetHead<site, (Route site -> [(Text, Text)] -> Text) -> Html>
public static <site> ToWidgetHead<site, Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html>> iToWidgetHeadHtmlUrl() {
    return (htmlf) -> (wdata) -> { iToWidgetHeadHtmlUrl(htmlf, wdata); return null; };
}
public static <site> void iToWidgetHeadHtmlUrl(Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html> htmlf, WidgetData<site> wdata) {
    Monoid<Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html>> iMonoid = iMonoidFun(iMonoidHtml);
    WidgetState<Route<site>> olds = wdata.getState();
    wdata.putState(WidgetState(olds.title, iMonoid.mappend(olds.head, htmlf), olds.body));
}

--  instance ToWidgetBody<site, (Route site -> [(Text, Text)] -> Text) -> Html>
public static <site> ToWidgetBody<site, Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html>> iToWidgetBodyHtmlUrl() {
    return (htmlf) -> (wdata) -> { iToWidgetBodyHtmlUrl(htmlf, wdata); return null; };
}
public static <site> void iToWidgetBodyHtmlUrl(Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html> htmlf, WidgetData<site> wdata) {
    iToWidgetHtmlUrl(htmlf, wdata);
}
---}

-- instances of 'Html'

instance ToWidget site Html where
    toWidget = toWidget . const

{---
--  instance ToWidgetHead<site, Html>
public static  <site> ToWidgetHead<site,Html> iToWidgetHeadHtml() {
    ToWidgetHead<site,Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html>> iTWHHtmlUrl = iToWidgetHeadHtmlUrl();
    return (html) -> iTWHHtmlUrl.toWidgetHead(_f -> html);
}

--  instance ToWidgetBody<site, Html>
public static <site> ToWidgetBody<site,Html> iToWidgetBodyHtml() {
    ToWidgetBody<site,Fun<Fun2<Route<site>, IList<Pair<Text,Text>>, Text>, Html>> iTWBHtmlUrl = iToWidgetBodyHtmlUrl();
    return (html) -> iTWBHtmlUrl.toWidgetBody(_f -> html);
}
---}

-- instances of 'Text'

instance ToWidget site Text where
    toWidget = toWidget . toHtml


-------------------------------------------------------------------------------
-- Page construction from Widget

data PageContent route = PageContent
        { pcTitle :: Html
        , pcHead :: UrlRender route -> Html
        , pcBody :: UrlRender route -> Html
        }

widgetToPageContent :: Widget site -> HandlerFor site (PageContent (Route site))
widgetToPageContent widget = do
    (_, wd) <- runWriterT (unWidgetFor widget)
    pure $ PageContent (maybe mempty id $ wdTitle wd) (wdHead wd) (wdBody wd)

