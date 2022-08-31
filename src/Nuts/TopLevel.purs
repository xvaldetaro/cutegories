module Nuts.TopLevel where

import Prelude

import App.Env (AppEvent(..), Env, mkEnv)
import App.Navigation (routeChangeEvent)
import App.Route (Route)
import App.Route as Route
import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Deku.Control (switcher, text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do as Doku
import FRP.Event (filterMap, fromEvent, keepLatest)
import Nuts.Debug as Debug
import Nuts.Game.GameNut as GameNut
import Nuts.Landing (nut) as Landing
import Nuts.Nav as Nav
import Nuts.Room.RoomNut as RoomNut
import Paraglider.Operator.Combine (combineLatest)
import Paraglider.Operator.FromAff (fromAff)
import Platform.Deku.Html (bangCss, bangId)
import Platform.Deku.Misc (envyBurning, wrapLogs)

nut :: Nut
nut = Doku.do
  sharedRouteEv <- envyBurning $ wrapLogs "sub" "unsub" routeChangeEvent
  eiEnvEv <- envyBurning $ fromEvent $ fromAff $ mkEnv
  let
    routeToChild :: Tuple Route Env -> Nut
    routeToChild (Tuple route env) = case route of
      Route.Landing -> Landing.nut env
      Route.Room roomId -> RoomNut.nut env roomId
      Route.Game gameId -> GameNut.nut env gameId
      Route.Debug -> Debug.nut env
      _ -> text_ "Route not available"

    appErrEv = keepLatest $ eiEnvEv <#> \eiEnv -> case eiEnv of
      Left e -> pure $ "Error loading Env: " <> show e
      Right {appEvent} -> appEvent # filterMap \action -> case action of
        ShowAppError e -> Just e
        -- _ -> Nothing

    envEv = eiEnvEv # filterMap (either (const Nothing) Just)

  D.div (bangId "TopLevel" <|> bangCss "flex flex-col h-screen text-gray-100 bg-gray-700")
    [ D.div (bangCss "text-lg text-red-500") [text $ appErrEv]
    , Nav.nut sharedRouteEv
    , switcher D.div (bangId "Router" <|> bangCss "h-full overflow-y-auto") routeToChild
        (combineLatest Tuple sharedRouteEv envEv)
    ]