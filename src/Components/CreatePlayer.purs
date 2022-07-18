module Components.CreatePlayer where

import Prelude

import Data.Tuple.Nested ((/\))
import HTML.Utils (css)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

component :: ∀ q m. H.Component q Unit Void m
component = Hooks.component \_ _ -> Hooks.do
  name /\ nameId <- Hooks.useState ""

  let handleNameChange str = Hooks.modify_ nameId (const str)

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
      ]