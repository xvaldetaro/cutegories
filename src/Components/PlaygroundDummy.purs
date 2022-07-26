module Components.PlaygroundDummy where

import Prelude

import App.Capa.Navigate (class Navigate)
import App.Store.MyStore as MS
import Components.Dumb.Styles (buttonCss', cardCss')
import Data.Either (Either(..), hush, isLeft, isRight)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Dumb.Break as Break
import Dumb.Input as Input
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore)
import Halogen.Subscription as HS
import Models.Models (Player(..))
import Platform.Firebase.Firestore (FSError, addDoc, getDoc, getDocs, observeDoc, setDoc)
import Platform.Html.CssUtils (css)
import Platform.Html.Utils (maybeElem)
import Platform.Misc.Disposable (disposeE, disposeM)

component
  :: ∀ q m. Navigate m => MonadAff m => MonadStore MS.Action MS.Store m => H.Component q Unit Void m
component = Hooks.component \_ _ -> Hooks.do
  path /\ pathId <- Hooks.useState "players"
  name /\ nameId <- Hooks.useState "player#"
  id /\ idId <- Hooks.useState ""
  result /\ resultId <- Hooks.useState ""
  resultError /\ resultErrorId <- Hooks.useState Nothing
  disposable /\ disposableId <- Hooks.useState Nothing

  let handlePath str = Hooks.put pathId str
  let handleName str = Hooks.put nameId str
  let handleId str = Hooks.put idId str
  let
    handleEdit _ = do
      { fb } <- getStore
      res <- H.liftAff $ setDoc fb.db path id {name}
      Hooks.put resultId $ show res
    handleClick _ = do
      { fb } <- getStore
      res <- H.liftAff $ addDoc fb.db path {name}
      Hooks.put resultId $ show res
    handleLoad _ = do
      { fb } <- getStore
      res :: (Either FSError Player) <- H.liftAff $ getDoc fb.db path id
      Hooks.put resultId $ show res
    handleLoadAll _ = do
      { fb } <- getStore
      res :: (Either FSError (Array Player)) <- H.liftAff $ getDocs fb.db path
      Hooks.put resultId $ show res

    handleObserve _ = do
      { fb } <- getStore
      { listener, emitter } <- H.liftEffect HS.create
      disposable' <- do
        let
          onNext = case _ of
            Left e -> HS.notify listener $
              Hooks.put resultErrorId $ Just $ "Error in emitted doc: " <> show e
            Right (doc :: Player) -> HS.notify listener $
              Hooks.put resultId $ "onNext: " <> show doc
        H.liftEffect $ observeDoc fb.db path id onNext (log "Completed")
      void $ Hooks.subscribe emitter
      Hooks.put disposableId $ Just disposable'

    handleDisposeObserve _ = do
      H.liftEffect $ disposeM disposable
      Hooks.put resultId "Disposed of subscription"

  Hooks.pure do
    HH.div [css "flex flex-col"]
      [ HH.div [css "flex flex-col w-96"]
        [ Input.input "Doc path:" "path" handlePath
        , HH.div [cardCss']
            [ Input.input "Name:" "name" handleName
            , Input.input "Id:" "name" handleId
            , Break.break
            , HH.div [buttonCss', HE.onClick handleClick] [ HH.text "Add Player"]
            , HH.div [buttonCss', HE.onClick handleEdit] [ HH.text "Edit Player Name"]
            , HH.div [buttonCss', HE.onClick handleLoad] [ HH.text "Load"]
            , HH.div [buttonCss', HE.onClick handleObserve] [ HH.text "Observe"]
            , Break.break
            , HH.div [buttonCss', HE.onClick handleDisposeObserve] [ HH.text "Dispose Observe"]
            , HH.div [buttonCss', HE.onClick handleLoadAll] [ HH.text "Load All"]
            ]
        ]
      , HH.div [ css "mt-11" ]
        [ HH.text result
        , maybeElem resultError \e -> HH.div [css "text-red-600"] [HH.text e]
        ]
      ]