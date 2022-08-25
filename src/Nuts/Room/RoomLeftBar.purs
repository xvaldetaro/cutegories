module Nuts.Room.RoomLeftBar where

import Prelude

import App.Env (Env, Nut_)
import App.Navigation (navigate)
import App.Route as Route
import Control.Alt ((<|>))
import Data.Array (length)
import Data.Filterable (filter)
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.Listeners (click)
import Effect.Class.Console (log)
import FRP.Event (AnEvent)
import Models.Models (Room(..))
import Nuts.Dumb.Btn as Btn
import Platform.Deku.Html (bangCss, combineCss, css)

nut :: ∀ s m l p. Env m -> AnEvent m Room -> Nut_ s m l p
nut env roomEv = D.div (bangCss "bg-gray-800 w-64 flex px-3 flex-col")
  [ D.div_ [text $ (\(Room {id}) -> "Room #: " <> id) <$> roomEv]
  , controls
  ]
  where
  -- TODO create game in Firebase
  createGame room = if hasEnoughPlayers room then
    log "creating game effect" *> (navigate $ Route.Game "createdGameId")
    else pure unit

  hasEnoughPlayers (Room {players}) = length players > 3
  canStartText can = if can then "Click to Start the game" else "Need more players to start"

  controls = D.div (bangCss "flex flex-col w-full")
    [ D.div (bangCss "") [text $ canStartText <<< hasEnoughPlayers <$> roomEv ]
    , D.button
      ( (click $ createGame <$> roomEv)
          <|> combineCss
              [ pure (Btn.baseCss <> Btn.tealCss <> css "w-full")
              , (if _ then "" else css "hover:bg-red-100 bg-red-100 cursor-not-allowed") <<< hasEnoughPlayers
                  <$> roomEv
              ]
      )
      [text_ "Start Game"]
    ]