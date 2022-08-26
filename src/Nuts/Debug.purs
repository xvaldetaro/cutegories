module Nuts.Debug where


import Prelude

import App.Env (Env)
import Core.Room.RoomManager (addPlayerToRoom, observeRoomPlayers, rmPlayerFromRoom)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text)
import Deku.Core (Domable)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Doku
import FRP.Event (ZoraEvent)
import Models.Models (Chat)
import Nuts.Dumb.Btn as Btn
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (wildSwitcher)
import Platform.FRP.FirebaseFRP (collectionEvent)
import Platform.FRP.Wild (WildEvent, unliftHappy)
import Platform.Firebase.Firestore (FSError)

nut :: ∀ l p. Env -> Domable l p
nut {fb} = Doku.do
  ps /\ sev <- useState'
  let add = addPlayerToRoom fb "asdf1" "mvSmbaF0wDC5DhcG5Ic0" ps
  let rm = rmPlayerFromRoom fb "asdf1" "mvSmbaF0wDC5DhcG5Ic0" ps
  D.div_
    [ text $ show <$> sev
    , Btn.teal "Join" "" (pure $ add)
    , Btn.teal "Leave" "" (pure $ rm)
    ]
