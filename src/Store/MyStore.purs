module Store.MyStore where

import Firebase.Firebase (FirebaseEnv)
import Models.Player (Player)

type Store = { fb :: FirebaseEnv, players :: Array Player }

data Action
  = SetPlayers (Array Player)

initialStore :: FirebaseEnv -> Store
initialStore fb = {fb, players: []}

reduce :: Store -> Action -> Store
reduce s a = case a of
  SetPlayers p -> s { players = p }