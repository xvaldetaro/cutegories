module Components.PlaygroundDummy where

import Prelude

import App.Capa.Navigate (class Navigate, navigate)
import App.Route as Components
import App.Route as Route
import App.Store.MyStore as MS
import Components.Dumb.Styles (buttonCss, buttonCss', cardCss)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
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
import Platform.Firebase.Firestore (addDoc, getPlayersAff)
import Platform.Html.CssUtils (css)

component
  :: ∀ q m. Navigate m => MonadAff m => MonadStore MS.Action MS.Store m => H.Component q Unit Void m
component = Hooks.component \_ _ -> Hooks.do
  path /\ pathId <- Hooks.useState "players"
  name /\ nameId <- Hooks.useState "player#"
  result /\ resultId <- Hooks.useState ""


  let handlePath str = Hooks.put pathId str
  let handleName str = Hooks.put nameId str
  let
    handleClick _ = do
      { fb } <- getStore
      res <- H.liftAff $ addDoc fb.db path {name}
      Hooks.put resultId $ show res

  Hooks.pure do
    HH.div [css "flex flex-row"]
      [ HH.div [css "flex flex-col w-96"]
        [ Input.input "Doc path:" "path" handlePath
        , Input.input "Name:" "name" handleName
        , HH.div [buttonCss', HE.onClick handleClick] [ HH.text "Save"]
        ]
      , HH.div [ css "mt-11" ]
        [ HH.text result

        ]
      ]