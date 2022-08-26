module Nuts.Room.RoomPlayerList where

import Prelude

import App.Env (Env)
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (oneOfMap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Deku.Control (dyn, text_)
import Deku.Core (Nut, insert_, remove)
import Deku.DOM as D
import FRP.Event (ZoraEvent, filterMap, mapAccum)
import Models.Models (Player, Room)
import Nuts.Dumb.Btn as Btn
import Paraglider.Operator.SwitchMap (switchMap)
import Platform.Deku.Html (bangCss, combineCss, css)

nut :: Env -> ZoraEvent Room -> ZoraEvent (Array Player) -> Nut
nut {fb: {myId}} roomEv playersEv =
  dyn D.ul (bangCss "flex flex-col") (mkRow <$> playersAddedUnfoldedEv)

  where
  mkRow player@{id, name} = (pure $ insert_ $ rowUi player) <|> mkOnSelfRemovedEv id

  playersAddedUnfoldedEv = playersDiffEv # switchMap \{addedPlayers} ->
    oneOfMap pure addedPlayers

  mkOnSelfRemovedEv selfId = playersDiffEv # filterMap \{removedPlayerIds} ->
    if Set.member selfId removedPlayerIds then Just remove else Nothing

  playersDiffEv :: ZoraEvent {addedPlayers :: Array Player, removedPlayerIds :: Set String, i:: String}
  playersDiffEv = mapAccum goAccumPlayers playersEv Map.empty

  goAccumPlayers players lastMap =
    let
        incomingMap = Map.fromFoldable $ (\player@{id} -> Tuple id player) <$> players
        newPlayers = Map.difference incomingMap lastMap
        removedPlayers = Map.difference lastMap incomingMap
    in Tuple
      lastMap
      { addedPlayers: Array.fromFoldable $ Map.values newPlayers
      , removedPlayerIds: Map.keys removedPlayers
      , i: show $ Array.fromFoldable $ Map.values incomingMap
      }

  rowUi {id, name} =
    let hiddenCss {admin} = if (admin /= myId) || (id == myId) then css "hidden" else "" in
    let nameCss = if id == myId then (css "text-white") else (css "text-gray-300") in
    D.li (bangCss "px-3 py-2 font-medium flex justify-between")
      [ D.div (bangCss nameCss) [text_ $ name]
      , D.button
        ( -- ( click $ pure (log $ "Removing " <> id) )
           (combineCss
                [ pure $ Btn.baseCss
                    <> css "hover:bg-red-600 bg-red-800 text-slate-100 rounded-full px-2"
                , hiddenCss <$> roomEv
                ]
              )
        ) [text_ "-"]
      ]
