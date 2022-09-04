module Nuts.Room.ResultsEnv where

import App.Env (Env)
import FRP.Event (Event)
import Models.Models (Player, Room, Game)

type ResultsEnv =
  { env :: Env
  , roomId :: String
  , roomEv :: Event Room
  , playersEv :: Event (Array Player)
  , game :: Game
  }