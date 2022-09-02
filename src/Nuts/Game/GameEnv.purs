module Nuts.Room.GameEnv where


import App.Env (Env)
import FRP.Event (ZoraEvent)
import Models.Models (Chat, Player, Room, Game)

type GameEnv =
  { env :: Env
  , roomId :: String
  , roomEv :: ZoraEvent Room
  , gameEv :: ZoraEvent Game
  }