module Store.MyStore where

import Prelude

import Firebase.Firebase (FirebaseEnv)
import Models.Player (Player)

type Store = { fb :: FirebaseEnv, players :: Array Player, count :: Int }

data Action
  = SetPlayers (Array Player)
  | Incr

initialStore :: FirebaseEnv -> Store
initialStore fb = {fb, players: [], count: 0}

reduce :: Store -> Action -> Store
reduce s a = case a of
  SetPlayers p -> s { players = p }
  Incr -> s