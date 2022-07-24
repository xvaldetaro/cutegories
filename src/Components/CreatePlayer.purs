module Components.CreatePlayer where

import Prelude

import App.Capa.Navigate (class Navigate, navigate)
import App.Route (Route(..))
import App.Store.MyStore as MS
import Data.Tuple.Nested ((/\))
import Dumb.Button as Dumb
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore)
import Models.Models (Player(..))
import Platform.Firebase.Firestore (addDoc)
import Platform.Html.CssUtils (css)

component
  :: ∀ q m
   . MonadStore MS.Action MS.Store m
  => Navigate m
  => MonadEffect m
  => MonadAff m
  => H.Component q Unit Void m
component = Hooks.component \_ _ -> Hooks.do
  name /\ nameId <- Hooks.useState ""

  let
    handleNameChange str = Hooks.modify_ nameId (const str)
    handleClick :: HookM m Unit
    handleClick = do
      {fb} <- getStore
      void $ H.liftAff $ addDoc fb.db "players" (Player {name, id: "asdf"})
      navigate PlayerList

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