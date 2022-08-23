module Platform.FRP.FirebaseFRP where

import Prelude

import Deku.Core (class Korok)
import FRP.Event (fromEvent, makeEvent)
import Platform.FRP.Wild (WildEvent, liftWildWithLoading)
import Platform.Firebase.Firestore (FSError, Firestore, observeDoc)
import Simple.JSON (class ReadForeign)

docEvent
  :: ∀ s m a
   . Korok s m
   => ReadForeign a
   => Firestore
   -> String
   -> String
   -> WildEvent m FSError a
docEvent fs path id =
  liftWildWithLoading
    $ fromEvent
      $ makeEvent \dsPush -> observeDoc fs path id dsPush (pure unit)