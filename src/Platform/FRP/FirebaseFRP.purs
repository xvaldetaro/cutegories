module Platform.FRP.FirebaseFRP where

import Prelude

import Data.Either (Either)
import FRP.Event (ZoraEvent, fromEvent, makeEvent)
import Paraglider.Operator.FromAff (fromAff)
import Platform.FRP.Wild (WildEvent, liftWildWithLoading)
import Platform.Firebase.Firestore (DocumentReference, FSError, Firestore, QueryConstraint, addDoc, observeCollection, observeDoc)
import Simple.JSON (class ReadForeign, class WriteForeign)

docEvent
  :: ∀ a
   . ReadForeign a
   => Firestore
   -> String
   -> String
   -> WildEvent FSError a
docEvent fs path id =
  liftWildWithLoading
    $ fromEvent
      $ makeEvent \dsPush -> observeDoc fs path id dsPush (pure unit)

collectionEvent
  :: ∀ a
   . ReadForeign a
   => Firestore
   -> String
   -> Array QueryConstraint
   -> WildEvent FSError (Array a)
collectionEvent fs path ctrs =
  liftWildWithLoading
    $ fromEvent
      $ makeEvent \dsPush -> observeCollection fs path ctrs dsPush

addDocEvent
  :: ∀ a
   . WriteForeign a
   => Firestore
   -> String
   -> a
   -> ZoraEvent (Either FSError DocumentReference)
addDocEvent fs path x = fromEvent <<< fromAff $ addDoc fs path x
