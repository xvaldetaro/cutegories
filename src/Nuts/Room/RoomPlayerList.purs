module Nuts.Room.RoomPlayerList where

import Prelude

import Control.Alt ((<|>))
import Core.Room.RoomManager (rmPlayerFromRoom)
import Data.Array as Array
import Data.Foldable (oneOfMap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Deku.Control (dyn, text, text_)
import Deku.Core (Nut, insert_, remove)
import Deku.DOM as D
import Deku.Listeners (click)
import Effect.Aff (launchAff_)
import FRP.Event (ZoraEvent, filterMap, mapAccum)
import Models.Models (Player)
import Nuts.Dumb.Btn as Btn
import Nuts.Room.RoomEnv (RoomEnv)
import Paraglider.Operator.SwitchMap (switchMap)
import Platform.Deku.Html (bangCss, combineCss, css)

nut :: RoomEnv -> Nut
nut { env: {fb, self}, roomId, roomEv, playersEv} =
  dyn D.ul (bangCss "flex flex-col") (mkRow <$> playersAddedUnfoldedEv)

  where
  myId = (_.uid) $ unwrap self
  mkRow player@{id} = (pure $ insert_ $ rowUi player) <|> mkOnSelfRemovedEv id

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

  kickPlayer id = launchAff_ $ void $ rmPlayerFromRoom fb roomId id
  rowUi {userId, name, id} =
    let hiddenCss {admin} = if (admin /= myId) || (userId == myId) then css "hidden" else "" in
    let nameCss = if userId == myId then (css "text-white") else (css "text-gray-300") in
    D.li (bangCss "px-3 py-2 font-medium flex justify-between")
      [ D.div (bangCss nameCss) [text_ name]
      , D.button
        ( (click $ pure (kickPlayer id))
            <|> (combineCss
                [ pure $ Btn.baseCss
                    <> css "hover:bg-red-600 bg-red-800 text-slate-100 rounded-full px-2"
                , hiddenCss <$> roomEv
                ]
              )
        ) [text_ "-"]
      ]
