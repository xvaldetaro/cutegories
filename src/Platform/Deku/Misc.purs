module Platform.Deku.Misc where

import Prelude

import Bolson.Core as Bolson
import Data.Foldable (class Foldable)
import Data.Monoid.Always (class Always)
import Data.Traversable (class Traversable, sequence)
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

usingEffect
  :: forall s m lock logic obj a
   . Korok s m
  => Always (m Unit) (Effect Unit)
  => m a
  -> (a -> Bolson.Entity logic obj m lock)
  -> Bolson.Entity logic obj m lock
usingEffect effect f =
  Bolson.EventfulElement' (Bolson.EventfulElement (fromEffect (f <$> effect)))

using'
  :: forall s m lock logic obj f a
   . Korok s m
  => Always (m Unit) (Effect Unit)
  => Traversable f
  => f (m a)
  -> (f a -> Bolson.Entity logic obj m lock)
  -> Bolson.Entity logic obj m lock
using' effects f =
  Bolson.EventfulElement' (Bolson.EventfulElement (fromEffect (f <$> (sequence effects))))