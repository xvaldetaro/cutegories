module Nuts.Debug where


import Prelude

import App.Env (Env)
import Deku.Control (text)
import Deku.Core (Domable)
import Deku.DOM as D
import FRP.Event (ZoraEvent)
import Models.Models (Chat)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (wildSwitcher)
import Platform.FRP.FirebaseFRP (collectionEvent)
import Platform.FRP.Wild (WildEvent, unliftDone)
import Platform.Firebase.Firestore (FSError)

nut :: ∀ l p. Env -> Domable l p
nut {fb} =
  let (wmessages :: WildEvent FSError Chat) = collectionEvent fb.db "rooms/mvSmbaF0wDC5DhcG5Ic0/messages" in

  wmessages # wildSwitcher (bangCss "h-full") \_ -> happy (unliftDone wmessages)
  where
  happy :: ZoraEvent Chat -> Domable l p
  happy messagesEv =
    D.div_
      [ text $ show <$> messagesEv
      ]
