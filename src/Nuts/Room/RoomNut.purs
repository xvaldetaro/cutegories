module Nuts.Room.RoomNut where

import Prelude

import App.Env (Env, forceDocPresent)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Core.Room.GameManager (observeGame)
import Core.Room.RoomManager (observeChat, observeRoom, observeRoomPlayers)
import Data.Array (find)
import Data.Maybe (isJust)
import Deku.Control (switcher, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do as Doku
import FRP.Event (Event)
import Models.Models (GameState(..), RoomId)
import Models.Paths (formsPersistPath)
import Nuts.Game.GameNut as GameNut
import Nuts.Game.ResultsNut as ResultsNut
import Nuts.Room.JoinRoomBlock as JoinRoomBlock
import Nuts.Waiting.Waiting as Waiting
import Paraglider.Operator.Combine (combineLatest3)
import Paraglider.Operator.DistinctUntilChanged (distinctUntilChangedBy)
import Paraglider.Operator.FromAff (fromAff)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (useCleanFbEvent)
import Platform.Firebase.Auth (uid)

nut :: Env -> RoomId -> Nut
nut env@{ fb, self } roomId = Doku.do
  roomEv <- useCleanFbEvent env $ forceDocPresent <$> observeRoom fb roomId
  playersEv <- useCleanFbEvent env $ observeRoomPlayers fb roomId
  -- chatEv <- useCleanFbEvent env $ observeChat fb roomId
  gameEv <- useCleanFbEvent env $ forceDocPresent <$> observeGame fb roomId

  let
    isInRoomEv :: Event Boolean
    isInRoomEv = playersEv <#> (isJust <<< find (\{id} -> id == uid self))

    roomEnv = { env , roomId , roomEv , playersEv, gameEv, chatEv: empty }

    gameStateChangedEv = distinctUntilChangedBy (_.gameState) gameEv
    renderEv = combineLatest3 render gameStateChangedEv roomEv isInRoomEv

    render game room isInRoom = if not isInRoom then JoinRoomBlock.nut env room else
      case game.gameState of
        NotStarted -> Waiting.nut roomEnv
        Started -> GameNut.nut { game, env, roomId, roomEv, gameEv, playersEv }
        Results -> ResultsNut.nut { env, roomId, roomEv, playersEv, game }
    renderWithLoadingEv = renderEv <|> pure loadingDiv

  switcher D.div (bangCss "h-full w-full bg-gray-800") identity renderWithLoadingEv

  where
  loadingDiv = D.div_ [text_ "Loading..."]


