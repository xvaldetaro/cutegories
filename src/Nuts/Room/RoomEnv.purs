module Nuts.Room.RoomEnv where


import App.Env (Env)
import FRP.Event (Event)
import Models.Models (Chat, Player, Room, Game)

type RoomEnv =
  { env :: Env
  , roomId :: String
  , roomEv :: Event Room
  , playersEv :: Event (Array Player)
  , chatEv :: Event Chat
  , gameEv :: Event Game
  }