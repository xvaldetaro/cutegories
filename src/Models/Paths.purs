module Models.Paths where

import Prelude

import Models.Models (RoomId)

gamePath :: String
gamePath = "games"

playersPath :: RoomId -> String
playersPath roomId = "rooms/" <> roomId <> "/players"

roomPath :: String
roomPath = "rooms"

chatPath :: RoomId -> String
chatPath roomId = "rooms/" <> roomId <> "/messages"

guessesPath :: String
guessesPath = "guesses"

valuationPath :: String
valuationPath = "valuations"
