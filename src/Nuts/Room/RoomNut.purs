module Nuts.Room.RoomNut where

import Prelude

import App.Env (Env, forceDocPresent)
import App.Navigation (navigate)
import App.Route as Route
import Control.Alt ((<|>))
import Core.Room.GameManager (observeGame)
import Core.Room.RoomManager (observeChat, observeRoom, observeRoomPlayers)
import Data.Array (find)
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Deku.Control (switcher, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do as Doku
import Models.Models (GameState(..), RoomId)
import Nuts.Game.GameNut as GameNut
import Nuts.Game.ResultsNut as ResultsNut
import Nuts.Room.RoomChat as RoomChat
import Nuts.Room.RoomLeftBar as RoomLeftBar
import Nuts.Room.RoomRightBar as RoomRightBar
import Paraglider.Operator.Combine (combineLatest)
import Paraglider.Operator.DoOnNext (doOnNext)
import Paraglider.Operator.InitialIfAsync (initialIfAsync)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (useCleanFbEvent)
import Platform.Firebase.Auth (uid)

nut :: Env -> RoomId -> Nut
nut env@{ fb, self } roomId = Doku.do
  roomEv <- useCleanFbEvent env $ forceDocPresent <$> observeRoom fb roomId
  playersEv' <- useCleanFbEvent env $ observeRoomPlayers fb roomId
  chatEv <- useCleanFbEvent env $ observeChat fb roomId
  gameEv <- useCleanFbEvent env $ forceDocPresent <$> observeGame fb roomId

  let
    playersEv = playersEv' # doOnNext \players -> case find (\{id} -> id == uid self) players of
      Nothing -> navigate Route.Landing
      Just _ -> pure unit

    roomEnv =
      { env
      , roomId
      , roomEv: roomEv
      , playersEv: playersEv
      , chatEv: chatEv
      }

    roomPage =
      D.div (bangCss "flex h-full items-stretch")
        [ RoomLeftBar.nut roomEnv
        , RoomChat.nut roomEnv
        , RoomRightBar.nut roomEnv
        ]

    gameEnv = { env, roomId, roomEv, gameEv, playersEv }
    gamePage = GameNut.nut gameEnv

    loadingOrDataEv = initialIfAsync Nothing $ Just <$> roomEv

  (combineLatest Tuple loadingOrDataEv gameEv) # switcher D.div (bangCss "h-full items-center justify-center") case _ of
    Tuple Nothing _ -> loadingDiv
    Tuple _ game -> case game.gameState of
      NotStarted -> roomPage
      Started -> gamePage
      Results -> ResultsNut.nut { env, roomId, roomEv, playersEv, game }

  where
  loadingDiv = D.div_ [text_ "Loading..."]


