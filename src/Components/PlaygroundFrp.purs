module Components.PlaygroundFrp where

import Prelude

import App.Capa.Navigate (class Navigate)
import App.Store.MyStore as MS
import Components.Dumb.Styles (buttonCss', cardCss, cardCss')
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Dumb.Input as Input
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import FRP.Event (Event)
import Halogen (SubscriptionId)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (unsubscribe)
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore)
import HyruleRx as Rx
import Models.Models (Player)
import Platform.Firebase.Firestore (FSError)
import Platform.Firebase.Firestore as FSError
import Platform.Html.CssUtils (css)
import Platform.Rx.FirebaseExt (docEvent)

component
  :: ∀ q m. Navigate m => MonadAff m => MonadStore MS.Action MS.Store m => H.Component q Unit Void m
component = Hooks.component \_ _ -> Hooks.do
  id /\ idId <- Hooks.useState ""
  eiCurrDoc /\ currDocId <- Hooks.useState $ Left $ FSError.ApiError ""
  journal /\ journalId <- Hooks.useState []
  docEventSub /\ docEventSubRef <- Hooks.useRef Nothing

  let handleId str = Hooks.put idId str
  let
    handleObserve _ = do
      { fb } <- getStore
      let (event :: Event (Either FSError Player)) = docEvent fb.db "players" id
      (subId :: SubscriptionId ) <- Hooks.subscribe $ Rx.toHalo $ Hooks.put currDocId <$> event
      H.liftEffect $ Ref.write (Just subId) docEventSubRef

    handleDisposeObserve _ = do
      mbDocEventSub :: Maybe SubscriptionId <- H.liftEffect $ Ref.read docEventSubRef
      for_ mbDocEventSub unsubscribe
      Hooks.modify_ journalId (flip snoc "Disposed of subscription")

  Hooks.pure do
    HH.div [css "flex flex-col"]
      [ HH.div [css "flex flex-col w-96"]
        [ HH.div [cardCss']
            [ Input.input "Id:" "id" handleId
            , HH.div [buttonCss', HE.onClick handleObserve] [ HH.text "Observe"]
            , HH.div [buttonCss', HE.onClick handleDisposeObserve] [ HH.text "Dispose Observe"]
            ]
        ]
      , HH.div [ cardCss "mt-11" ]
        [ case eiCurrDoc of
          Left e -> HH.div [css "text-red-600"] [HH.text $ show e]
          Right d -> HH.text $ "Current doc:" <> show d
        ]
      , HH.div [ cardCss "mt-11" ] (journal <#> HH.text)
      ]