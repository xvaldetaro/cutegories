module Platform.Html.Utils where

import Prelude

import App.Route (Route, routeCodec)
import Routing.Duplex (print)

-- | We must provide a `String` to the "href" attribute, but we represent routes with the much
-- | better `Route` type. This utility is a drop-in replacement for `href` that uses `Route`.
safeHref :: Route -> String
safeHref = append "#" <<< print routeCodec
