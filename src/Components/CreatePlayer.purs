module Components.CreatePlayer where

import Prelude

import Core.Capa.Navigate (class Navigate, navigate)
import Core.Route (Route(..))
import Data.Tuple.Nested ((/\))
import Dumb.Button as Dumb
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import HTML.Utils (css)
import Halogen (lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

component :: ∀ q m. Navigate m => MonadEffect m => H.Component q Unit Void m
component = Hooks.component \_ _ -> Hooks.do
  name /\ nameId <- Hooks.useState ""

  let
    handleNameChange str = Hooks.modify_ nameId (const str)
    handleClick = do
      lift $ navigate PlayerList

  -- handleClick = do
  --   status <- Hooks.request slotToken _button unit Button.IsOn
  --   Hooks.put buttonStatusId status
  Hooks.pure do
    HH.div_
      [ HH.input
          [ css ""
          , HE.onValueInput handleNameChange
          , HP.value name
          , HP.type_ HP.InputText
          , HP.placeholder "Player Name"
          ]
      , Dumb.button "Create" handleClick
      ]