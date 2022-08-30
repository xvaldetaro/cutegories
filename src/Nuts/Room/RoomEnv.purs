module Nuts.Room.RoomEnv where


import App.Env (Env)
import FRP.Event (ZoraEvent)
import Models.Models (Chat, Room, Player)

type RoomEnv =
  { env :: Env
  , roomId :: String
  , roomEv :: ZoraEvent Room
  , playersEv :: ZoraEvent (Array Player)
  , chatEv :: ZoraEvent Chat
  }