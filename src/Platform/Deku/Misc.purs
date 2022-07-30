module Platform.Deku.Misc where

import Prelude

import Bolson.Core as Bolson
import Data.Monoid.Always (class Always)
import Deku.Core (class Korok)
import Effect (Effect)
import Paraglider.AffBridge (fromEffect)

nutFromEffect
  :: forall s m lock logic obj
   . Korok s m
  => Always (m Unit) (Effect Unit)
  => m (Bolson.Entity logic obj m lock)
  -> Bolson.Entity logic obj m lock
nutFromEffect effect = Bolson.EventfulElement' (Bolson.EventfulElement (fromEffect effect))