module Platform.Rx.FirebaseExt where

import Prelude

import Data.Either (Either)
import FRP.Event (Event, makeEvent)
import Platform.Firebase.Firestore (FSError, Firestore, observeDoc)
import Simple.JSON (class ReadForeign)

docEvent :: ∀ a . ReadForeign a => Firestore -> String -> String -> Event (Either FSError a)
docEvent fs path id = makeEvent $ \k -> observeDoc fs path id k (pure unit)