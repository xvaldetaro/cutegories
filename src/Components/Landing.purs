module Components.Landing where

import Prelude

import Components.Dumb.Styles (buttonCss, cardCss)
import Core.Capa.Navigate (class Navigate, navigate)
import Core.Route as Components
import Core.Route as Route
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Dumb.Button as Dumb.Button
import Dumb.VerticalListClickable as Dumb.VerticalListClickable
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Firebase.Firestore (getPlayersAff)
import HTML.Utils (css)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore)
import Models.Player (Player(..))
import Store.MyStore as MS

type State = { players :: Maybe (Array Player) }
data Action = Initialize | CreatePlayerClick

component
  :: ∀ q m. Navigate m => MonadAff m => MonadStore MS.Action MS.Store m => H.Component q Unit Void m
component = Hooks.component \_ _ -> Hooks.do
  Hooks.pure do
    HH.div
      [ cardCss "flex-col flex w-96 items-center px-8" ]
      [ HH.div [ css "text-lg font-bold mb-6 mt-6 text-slate-500" ] [ HH.text "Create a Game" ]
      , HH.div [ buttonCss "w-full text-center" ] [ HH.text "Create" ]
      , break
      -- Join section
      , HH.div [ css "text-lg font-bold mb-4 text-slate-500" ] [ HH.text "Join a Game" ]
      , input
      , HH.div [ buttonCss "w-full text-center mb-8"] [ HH.text "Enter" ]
      ]
  where
  break = HH.div [css "flex flex-row w-full items-center justify-between my-4"]
    [ HH.div [ css "h-0 w-full border-t border-slate-300"] []
    , HH.div [ css "text-slate-400 mx-4"] [ HH.text "Or" ]
    , HH.div [ css "h-0 w-full border-t border-slate-300"] []
    ]
  inputId = "game-id"
  input = HH.fieldset [ css "flex flex-col w-full font-semibold text-slate-500 text-md mb-4"]
    [ HH.label [HP.for inputId, css "mb-2" ] [ HH.text "Game Room"]
    , HH.input
      [HP.id inputId
      , HP.placeholder "12345"
      , css "rounded-lg border border-slate-600 px-4 py-1"
      ]
    ]