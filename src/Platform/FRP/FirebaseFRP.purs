module Platform.FRP.FirebaseFRP where

import Prelude

import App.Env (FbEvent)
import FRP.Event (fromEvent, makeEvent)
import Paraglider.Operator.FromAff (fromAff)
import Platform.Firebase.Firestore.Common (Firestore, DocumentReference)
import Platform.Firebase.Firestore.DocRef (DocRef)
import Platform.Firebase.Firestore.Query (Query)
import Platform.Firebase.Firestore.Read (observeDoc, observeQueryCollection)
import Platform.Firebase.Firestore.Write (addDoc)
import Simple.JSON (class ReadForeign, class WriteForeign)

docEvent
  :: ∀ a
   . ReadForeign a
   => Firestore
   -> String
   -> String
   -> FbEvent a
docEvent fs path id = fromEvent $ makeEvent \dsPush -> observeDoc fs path id dsPush (pure unit)

collectionEvent
  :: ∀ a
   . ReadForeign a
   => Firestore
   -> Query
   -> FbEvent (Array a)
collectionEvent fs query = fromEvent $ makeEvent \dsPush ->
  observeQueryCollection fs query dsPush

addDocEvent
  :: ∀ a
   . WriteForeign a
   => Firestore
   -> String
   -> a
   -> FbEvent DocRef
addDocEvent fs path x = fromEvent <<< fromAff $ addDoc fs path x
