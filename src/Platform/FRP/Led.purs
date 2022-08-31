module Platform.FRP.Led where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal as Ref
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff, Error)
import FRP.Event (AnEvent, ZoraEvent, filterMap, fromEvent, keepLatest, makeEvent, subscribe)
import Hyrule.Zora (Zora)
import Paraglider.Operator.Combine (combineFold)
import Paraglider.Operator.DistinctUntilChanged (distinctUntilChanged)
import Paraglider.Operator.FromAff (fromAffSafe)
import Paraglider.Operator.MemoBeh (memoBeh)
import Paraglider.Operator.Replay (replayRefCount)
import Paraglider.Operator.Take (take)
import Paraglider.Operator.ToClosure (toClosure)

data Led e a = Led (ZoraEvent Boolean) (ZoraEvent e) (ZoraEvent a)

led :: ∀ e a. ZoraEvent e -> ZoraEvent a -> Led e a
led errorEv aEv = Led (loadingEvent aEv) errorEv aEv

led' :: ∀ e a. ZoraEvent (Either e a) -> Led e a
led' ev = Led (loadingEvent ev) (errorEvent ev) (dataEvent ev)

dataEvent :: ∀ e a. ZoraEvent (Either e a) -> ZoraEvent a
dataEvent ev = filterMap dataGo ev
  where
  dataGo (Right d) = Just d
  dataGo _ = Nothing

errorEvent :: ∀ e a. ZoraEvent (Either e a) -> ZoraEvent e
errorEvent ev = filterMap errorGo ev
  where
  errorGo (Left e) = Just e
  errorGo _ = Nothing

loadingEvent :: ∀ a. ZoraEvent a -> ZoraEvent Boolean
loadingEvent ev = distinctUntilChanged $ initialIfAsync true falseEv
  where
  falseEv = const false <$> ev

-- | If the event emits synchronously upon subscription then does nothing.
-- | Otherwise adds `initial` as a synchronous first emission to the event
initialIfAsync :: ∀ a. a -> ZoraEvent a -> ZoraEvent a
initialIfAsync initial ev = makeEvent \k -> do
  didEmitRef <- liftST $ Ref.new false
  sub <- subscribe ev \v -> do
    k v
    void $ liftST $ Ref.write true didEmitRef
  didEmit <- liftST $ Ref.read didEmitRef
  unless didEmit $ k initial
  pure sub