module Platform.Firebase.Firestore.Common where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Platform.Firebase.Config (FirebaseApp)

data Firestore

foreign import firestoreDb :: FirebaseApp -> Effect (Promise Firestore)

firestoreDbAff :: FirebaseApp -> Aff Firestore
firestoreDbAff = toAffE <<< firestoreDb

type DocumentReference =
  { id :: String
  , path :: String
  }

data DocumentSnapshot