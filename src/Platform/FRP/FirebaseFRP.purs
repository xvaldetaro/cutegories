module Platform.FRP.FirebaseFRP where

import Prelude

import Platform.Firebase.Synonyms (FbEvent)
import Data.Maybe (Maybe)
import FRP.Event (makeEvent)
import Paraglider.Operator.FromAff (fromAff)
import Platform.Firebase.Firestore.Common (Firestore)
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
   -> FbEvent (Maybe a)
docEvent fs path id = makeEvent \dsPush -> observeDoc fs path id dsPush

collectionEvent
  :: ∀ a
   . ReadForeign a
   => Firestore
   -> Query
   -> FbEvent (Array a)
collectionEvent fs query = makeEvent \dsPush ->
  observeQueryCollection fs query dsPush

addDocEvent
  :: ∀ a
   . WriteForeign a
   => Firestore
   -> String
   -> a
   -> FbEvent DocRef
addDocEvent fs path x = fromAff $ addDoc fs path x
