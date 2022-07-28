module Components.Landing where

import Prelude

import App.Capa.Navigate (class Navigate, navigate)
import App.Route as Components
import App.Route as Route
import App.Store.MyStore as MS
import Components.Dumb.Styles (buttonCss, cardCss, cardCss')
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Dumb.Break as Break
import Dumb.Button as Dumb.Button
import Dumb.Input as Input
import Dumb.VerticalListClickable as Dumb.VerticalListClickable
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore)
import Models.Models (Player(..))
import Platform.Html.CssUtils (css)
import Platform.Html.Utils (maybeElem)

component
  :: ∀ q m. Navigate m => MonadAff m => MonadStore MS.Action MS.Store m => H.Component q Unit Void m
component = Hooks.component \_ _ -> Hooks.do
  roomId /\ roomIdId <- Hooks.useState Nothing
  errorMsg /\ errorMsgId <- Hooks.useState Nothing

  let
    onEnterClick _ = case roomId of
      Nothing -> Hooks.put errorMsgId $ Just "Please input RoomId"
      Just roomId' -> navigate $ Route.Room roomId'

  Hooks.pure do
    HH.div_
      [ maybeElem errorMsg \e -> HH.div [ cardCss "text-red-700 w-96" ] [ HH.text e ]
      , HH.div
          [ cardCss "flex-col flex w-96 items-center px-8" ]
          [ HH.div [ css "text-lg font-bold mb-6 mt-6 text-slate-500" ] [ HH.text "Create a Game" ]
          , HH.div [ buttonCss "w-full text-center" ] [ HH.text "Create" ]
          , Break.break
          -- Join section
          , HH.div [ css "text-lg font-bold mb-4 text-slate-500" ] [ HH.text "Join a Game" ]
          , Input.input "Game Room Id:" "roomid" (Hooks.put roomIdId <<< Just)
          , HH.div
              [ buttonCss "w-full text-center mb-8"
              , HE.onClick onEnterClick
              ]
              [ HH.text "Enter" ]
          ]
      ]