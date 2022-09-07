module Nuts.TopLevel where

import Prelude

import App.Env (AppEvent(..), Env, mkEnv)
import App.Navigation (navigate, routeChangeEvent)
import App.Route (Route)
import App.Route as Route
import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (switcher, text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do as Doku
import Deku.Listeners (click)
import FRP.Event (filterMap, keepLatest)
import Nuts.Bank.BankNut as BankNut
import Nuts.Debug as Debug
import Nuts.Dumb.Modal (toast)
import Nuts.Landing (nut) as Landing
import Nuts.Nav as Nav
import Nuts.Room.RoomNut as RoomNut
import Paraglider.Operator.Combine (combineLatest)
import Paraglider.Operator.FromAff (fromAff)
import Paraglider.Operator.SwitchMap (switchMap)
import Paraglider.Operator.Timeout (timeout)
import Platform.Deku.Html (bangCss, bangId)
import Platform.Deku.Misc (envyBurning, wrapLogs)

nut :: Nut
nut = Doku.do
  sharedRouteEv <- envyBurning $ wrapLogs "sub" "unsub" routeChangeEvent
  eiEnvEv <- envyBurning $ fromAff $ mkEnv
  let
    routeToChild :: Tuple Route Env -> Nut
    routeToChild (Tuple route env) = case route of
      Route.Landing -> Landing.nut env
      Route.Room roomId -> RoomNut.nut env roomId
      Route.Bank -> BankNut.nut env
      Route.Debug -> Debug.nut env
      _ -> text_ "Route not available"

    appErrEv = keepLatest $ eiEnvEv <#> \eiEnv -> case eiEnv of
      Left e -> pure $ "Error loading Env: " <> show e
      Right {appEvent} -> appEvent # filterMap \action -> case action of
        ShowAppError e -> Just e
        _ -> Nothing

    envEv = eiEnvEv # filterMap (either (const Nothing) Just)
    appEv = switchMap (_.appEvent) envEv
    toastEv = appEv # filterMap case _ of
      ShowToast text duration -> Just $ text /\ duration
      _ -> Nothing
    toastOnOffEv = pure Nothing
      <|> (toastEv # switchMap \(text /\ duration) ->
            pure (Just text) <|> (const Nothing <$> timeout duration)
          )

  D.div (bangCss "w-full absolute inset-0 bg-gray-900")
    [
      D.div (bangId "TopLevel" <|> bangCss "mx-auto flex flex-col h-full max-w-md text-gray-100 bg-gray-700")
        [ D.div ((bangCss "text-lg text-red-500") <|> (click $ pure $ navigate Route.Landing))
            [ text $ appErrEv ]
        , toast (isJust <$> toastOnOffEv) (text $ fromMaybe "" <$> toastOnOffEv)
        , Nav.nut sharedRouteEv
        , switcher D.div (bangId "Router" <|> bangCss "h-full overflow-y-auto") routeToChild
            (combineLatest Tuple sharedRouteEv envEv)
        ]
    ]