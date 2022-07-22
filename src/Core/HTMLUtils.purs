module Core.HTMLUtils where

import Prelude

import Core.Route (Route, routeCodec)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)

-- | We must provide a `String` to the "href" attribute, but we represent routes with the much
-- | better `Route` type. This utility is a drop-in replacement for `href` that uses `Route`.
safeHref :: forall r i. Route -> HH.IProp (href :: String | r) i
safeHref = HP.href <<< append "#" <<< print routeCodec