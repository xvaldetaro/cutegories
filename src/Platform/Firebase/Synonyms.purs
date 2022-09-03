module Platform.Firebase.Synonyms where

import Data.Either (Either)
import Effect.Aff (Aff)
import FRP.Event (Event)
import Platform.Firebase.FbErr (FbErr)

type FbEvent a = Event (Either FbErr a)
type FbAff a = Aff (Either FbErr a)
