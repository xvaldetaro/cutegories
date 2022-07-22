module Components.Router where

import Prelude

import Components.CreatePlayer as CreatePlayer
import Components.Dumb.Icon as Icon
import Components.Landing as Landing
import Components.PlayerList as PlayerList
import Core.Capa.Navigate (class Navigate, navigate)
import Core.HTMLUtils (safeHref)
import Core.Route (Route(..), routeCodec)
import Core.Route as Route
import Data.Const (Const)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Dumb.Nav as Nav
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import HTML.Utils (css)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Subscription as HS
import Platform.OpaqueSlot (OpaqueSlot)
import Routing.Duplex (parse)
import Routing.Duplex as RD
import Routing.Hash (getHash, matchesWith)
import Store.MyStore as MS
import Type.Proxy (Proxy(..))

type ChildSlots =
  ( playerList :: OpaqueSlot Unit
  , createPlayer :: OpaqueSlot Unit
  , landing :: OpaqueSlot Unit
  )

type Query :: ∀ k. k -> Type
type Query = Const Void

type State =
  { route :: Maybe Route
  , hashSubscriptionDisposable :: Effect Unit
  }

data Action = Initialize | OnRouteChanged Route | Finalize

component
  :: ∀ q m
   . MonadAff m
  => Navigate m
  => MonadStore MS.Action MS.Store m
  => H.Component q Unit Void m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize, finalize = Just Finalize }
    }
  where
  initialState _ = { route: Nothing, hashSubscriptionDisposable: pure unit }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = HH.div
    [ HP.classes $
      [ ClassName "font-sans text-slate-700 min-h-screen w-screen"
      , ClassName "flex flex-col bg-[#faf9f9]"
      ]
    ]
    [ Nav.nav route
    , mainContent
    ]
    where
    mainContent = case route of
      Nothing -> HH.div [ css "text-red-300" ] [ HH.text "Invalid address." ]
      Just route' -> HH.div [ css "container mx-auto"]
        [ case route' of
            Landing -> HH.slot_ (Proxy :: _ "landing") unit Landing.component unit
            PlayerList -> HH.slot_ (Proxy :: _ "playerList") unit PlayerList.component unit
            CreatePlayer -> HH.slot_ (Proxy :: _ "createPlayer") unit CreatePlayer.component unit
        ]

  handleAction = case _ of
    Initialize -> do
      updateStore MS.Incr
      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter

      hashSubscriptionDisposable <- H.liftEffect $ matchesWith (parse routeCodec) \old new ->
        when (old /= Just new) $ HS.notify listener $ OnRouteChanged new

      H.modify_ _ { hashSubscriptionDisposable = hashSubscriptionDisposable }

      -- first we'll get the route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> H.liftEffect getHash
      -- then we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Landing initialRoute

    OnRouteChanged dest -> do
      { route } <- H.get
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
        H.modify_ _ { route = Just dest }

    Finalize -> do
      { hashSubscriptionDisposable } <- H.get
      H.liftEffect hashSubscriptionDisposable