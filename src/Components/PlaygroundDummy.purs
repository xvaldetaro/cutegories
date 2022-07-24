module Components.PlaygroundDummy where

import Prelude

import App.Capa.Navigate (class Navigate)
import App.Store.MyStore as MS
import Components.Dumb.Styles (buttonCss', cardCss')
import Data.Either (Either)
import Data.Tuple.Nested ((/\))
import Dumb.Break as Break
import Dumb.Input as Input
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore)
import Models.Models (Player)
import Platform.Firebase.Firestore (FSError, addDoc, getDoc, getDocs)
import Platform.Html.CssUtils (css)

component
  :: ∀ q m. Navigate m => MonadAff m => MonadStore MS.Action MS.Store m => H.Component q Unit Void m
component = Hooks.component \_ _ -> Hooks.do
  path /\ pathId <- Hooks.useState "players"
  name /\ nameId <- Hooks.useState "player#"
  id /\ idId <- Hooks.useState ""
  result /\ resultId <- Hooks.useState ""

  let handlePath str = Hooks.put pathId str
  let handleName str = Hooks.put nameId str
  let handleId str = Hooks.put idId str
  let
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

  Hooks.pure do
    HH.div [css "flex flex-col"]
      [ HH.div [css "flex flex-col w-96"]
        [ Input.input "Doc path:" "path" handlePath
        , HH.div [cardCss']
            [ Input.input "Name:" "name" handleName
            , HH.div [buttonCss', HE.onClick handleClick] [ HH.text "Save"]
            , Break.break
            , Input.input "Id:" "name" handleId
            , HH.div [buttonCss', HE.onClick handleLoad] [ HH.text "Load"]
            , Break.break
            , HH.div [buttonCss', HE.onClick handleLoadAll] [ HH.text "Load All"]
            ]
        ]
      , HH.div [ css "mt-11" ]
        [ HH.text result
        ]
      ]