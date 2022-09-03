module Nuts.Room.ResultsEnv where

import App.Env (Env)
import FRP.Event (Event)
import Models.Models (Game, Player, Room)

type ResultsEnv =
  { env :: Env
  , roomId :: String
  , roomEv :: Event Room
  , playersEv :: Event (Array Player)
  }