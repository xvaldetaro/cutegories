module Nuts.Room.GameEnv where


import App.Env (Env)
import FRP.Event (Event)
import Models.Models (Chat, Game, Room, Player)

type GameEnv =
  { env :: Env
  , game :: Game
  , roomId :: String
  , roomEv :: Event Room
  , gameEv :: Event Game
  , playersEv :: Event (Array Player)
  }