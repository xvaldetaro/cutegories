module Nuts.Room.RoomLeftBar where

import Prelude

import App.Navigation (navigate)
import App.Route as Route
import Control.Alt ((<|>))
import Data.Array (length)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Listeners (click)
import Effect.Class.Console (log)
import Nuts.Dumb.Btn as Btn
import Nuts.Room.RoomEnv (RoomEnv)
import Platform.Deku.Html (bangCss, combineCss, css)

nut :: RoomEnv -> Nut
nut { playersEv, roomId } =
  D.div (bangCss "bg-gray-800 w-64 flex px-3 flex-col")
    [ D.div_ [ text_ $ "Room #: " <> roomId ]
    , controls
    ]
  where
  -- TODO create game in Firebase
  createGame players =
    if hasEnoughPlayers players then
      log "creating game effect" *> (navigate $ Route.Game "createdGameId")
    else pure unit

  hasEnoughPlayers players = length players > 2
  canStartText can = if can then "Click to Start the game" else "Need more players to start"

  controls = D.div (bangCss "flex flex-col w-full")
    [ D.div (bangCss "") [ text $ canStartText <<< hasEnoughPlayers <$> playersEv ]
    , D.button
        ( (click $ createGame <$> playersEv)
            <|> combineCss
              [ pure (Btn.baseCss <> Btn.tealCss <> css "w-full")
              , (if _ then "" else css "hover:bg-red-100 bg-red-100 cursor-not-allowed") <<<
                  hasEnoughPlayers
                  <$> playersEv
              ]
        )
        [ text_ "Start Game" ]
    ]