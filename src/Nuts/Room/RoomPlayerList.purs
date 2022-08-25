module Nuts.Room.RoomPlayerList where

import Prelude

import App.Env (Nut_, Env)
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (oneOfMap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Deku.Control (dyn, text_)
import Deku.Core (insert_, remove)
import Deku.DOM as D
import FRP.Event (AnEvent, filterMap, mapAccum)
import Models.Models (Player(..), Room(..))
import Nuts.Dumb.Btn as Btn
import Paraglider.Operator.SwitchMap (switchMap)
import Platform.Deku.Html (bangCss, combineCss, css)

nut :: ∀ s m l p. Env m -> AnEvent m Room -> AnEvent m (Array Player) -> Nut_ s m l p
nut {fb: {myId}} roomEv playersEv =
  dyn D.ul (bangCss "flex flex-col") (mkRow <$> playersAddedUnfoldedEv)

  where
  mkRow player@(Player {id, name}) = (pure $ insert_ $ rowUi player) <|> mkOnSelfRemovedEv id

  playersAddedUnfoldedEv = playersDiffEv # switchMap \{addedPlayers} ->
    oneOfMap pure addedPlayers

  mkOnSelfRemovedEv selfId = playersDiffEv # filterMap \{removedPlayerIds} ->
    if Set.member selfId removedPlayerIds then Just remove else Nothing

  playersDiffEv :: AnEvent m {addedPlayers :: Array Player, removedPlayerIds :: Set String, i:: String}
  playersDiffEv = mapAccum goAccumPlayers playersEv Map.empty

  goAccumPlayers players lastMap =
    let
        incomingMap = Map.fromFoldable $ (\player@(Player {id}) -> Tuple id player) <$> players
        newPlayers = Map.difference incomingMap lastMap
        removedPlayers = Map.difference lastMap incomingMap
    in Tuple
      lastMap
      { addedPlayers: Array.fromFoldable $ Map.values newPlayers
      , removedPlayerIds: Map.keys removedPlayers
      , i: show $ Array.fromFoldable $ Map.values incomingMap
      }

  rowUi (Player {id, name}) =
    let hiddenCss (Room {admin}) = if (admin /= myId) || (id == myId) then css "hidden" else "" in
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
