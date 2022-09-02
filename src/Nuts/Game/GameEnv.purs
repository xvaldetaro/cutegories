module Nuts.Room.GameEnv where


import App.Env (Env)
import FRP.Event (Event)
import Models.Models (Chat, Player, Room, Game)

type GameEnv =
  { env :: Env
  , roomId :: String
  , roomEv :: Event Room
  , gameEv :: Event Game
  }