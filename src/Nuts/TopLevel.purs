module Nuts.TopLevel where

import Prelude

import App.Capa.Navigate (class Navigate, navigate)
import App.Env (Env)
import App.Route (Route(..), routeCodec)
import App.Route as Route
import Bolson.Control (switcher)
import Bolson.Core (bussed)
import Components.CreatePlayer as CreatePlayer
import Components.Landing as Landing
import Components.PlayerList as PlayerList
import Components.PlaygroundDummy as PlaygroundDummy
import Components.PlaygroundFrp as PlaygroundFrp
import Components.Room as Room
import Control.Alt ((<|>))
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Either (hush)
import Data.Foldable (oneOf, oneOfMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (lcmap)
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (class Korok, Nut, Domable)
import Deku.DOM as D
import Deku.Listeners (click)
import Dumb.Nav as Nav
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (bang, bus, makeEvent)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Platform.Html.CssUtils (css)
import Platform.Misc.OpaqueSlot (OpaqueSlot)
import Routing.Duplex (parse, print)
import Routing.Duplex as RD
import Routing.Hash (getHash, matchesWith, setHash)
import Type.Proxy (Proxy(..))

topLevel :: ∀ s l p. Korok s Effect => { param :: String } -> Domable Effect l p
topLevel { param } = bussed \push -> lcmap (bang "initial" <|> _) \event ->
  let
    routeEv = makeEvent \k ->
      matchesWith (parse routeCodec) \old new ->
        when (old /= Just new) $ k new

    navigate = setHash <<< print Route.routeCodec
  in
    D.div (bang $ D.Class := "flex flex-col")
      [ D.div
          ( oneOf
              [ bang $ D.Class := "text-red-700"
              , bang $ D.OnClick := cb (const $ push "clicked")
              ]
          )
          [ text (show <$> routeEv) ]
      , D.div (bang $ D.Class := "flex flex-row gap-4")
        [ D.div (bang $ D.OnClick := cb (const $ navigate Route.CreatePlayer)) [text_ "CreateP"]
        , D.div (bang $ D.OnClick := cb (const $ navigate Route.Landing)) [text_ "Landing"]
        ]
      ]