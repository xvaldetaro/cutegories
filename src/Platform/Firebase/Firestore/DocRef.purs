module Platform.Firebase.Firestore.DocRef where

import Prelude

import Data.Maybe (Maybe(..))
import Simple.JSON (class ReadForeign, class WriteForeign)
import Unsafe.Coerce (unsafeCoerce)

data DocRef
instance writeForeignForeign :: WriteForeign DocRef where
  writeImpl = unsafeCoerce

instance readForeign :: ReadForeign DocRef where
  readImpl = pure <<< unsafeCoerce

foreign import id_ :: DocRef -> String
foreign import path_ :: DocRef -> String
foreign import parent_ :: DocRef -> (∀ a. a -> Maybe a) -> (∀ a. Maybe a) -> Maybe DocRef

id :: DocRef -> String
id = id_

path :: DocRef -> String
path = path_

parent :: DocRef -> Maybe DocRef
parent d = parent_ d Just Nothing