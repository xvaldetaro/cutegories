module Components.Room where

import Prelude

import Components.Dumb.Styles (buttonCss, cardCss)
import App.Capa.Navigate (class Navigate, navigate)
import App.Route as Components
import App.Route as Route
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Dumb.Button as Dumb.Button
import Dumb.VerticalListClickable as Dumb.VerticalListClickable
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Platform.Html.CssUtils (css)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore)
import Models.Models (Player(..))
import App.Store.MyStore as MS

type Input = String

component
  :: ∀ q m. Navigate m => MonadAff m => MonadStore MS.Action MS.Store m => H.Component q Input Void m
component = Hooks.component \_ roomId -> Hooks.do

  Hooks.pure do
    HH.div
      [ cardCss "flex-col flex w-96 items-center px-8" ]
      [ HH.div [ css "text-lg font-bold mb-6 mt-6 text-slate-500" ] [ HH.text $ "Room: " <> roomId ]
    ]