module Platform.Rx.FirebaseExt where

import Prelude

import Data.Either (Either)
import FRP.Event (Event, makeEvent)
import Halogen.Subscription (Emitter, makeEmitter)
import Platform.Firebase.Firestore (FSError, Firestore, observeDoc)
import Simple.JSON (class ReadForeign)

hObserveDoc
  :: ∀ a
  . ReadForeign a
  => Firestore
  -> String
  -> String
  -> Emitter (Either FSError a)
hObserveDoc fs path id = makeEmitter \notifyF -> do
  observeDoc fs path id notifyF (pure unit)


-- suite "Event" (\i f -> f i) Event.create Event.subscribe