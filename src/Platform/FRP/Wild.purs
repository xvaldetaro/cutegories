module Platform.FRP.Wild where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (class MonadST)
import Data.Either (Either, either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import FRP.Event (AnEvent, filterMap, keepLatest)

-- | Wild abstracts data that comes "from the wild", i.e. data that needs to be loaded and can error
-- | Works like an Either with an extra "Loading" state
data Wild e a = Loading | Error e | Done a

derive instance genericWild :: Generic (Wild e a) _
instance showWild :: (Show e, Show a) => Show (Wild e a) where
  show = genericShow

instance eqWild :: (Eq e, Eq a) => Eq (Wild e a) where
  eq = genericEq

instance ordWild :: (Ord e, Ord a) => Ord (Wild e a) where
  compare = genericCompare

derive instance functorWild :: Functor (Wild e)
instance applicativeWild :: Applicative (Wild e) where
  pure x = Done x

instance bindWild :: Bind (Wild e) where
  bind m fm = case m of
    Done x -> fm x
    Loading -> Loading
    Error e -> Error e

instance monadWild :: Monad (Wild e)
instance applyWild :: Apply (Wild e) where
  apply = ap

-- | An Event of Wild data.
-- | Provides helpers for chaining sequential wild operations
newtype WildEvent m e a = WildEvent (AnEvent m (Wild e a))

instance functorWildEvent :: Functor (WildEvent m e) where
  map f (WildEvent ev) = WildEvent $ map (map f) ev

instance applyWildEvent :: MonadST s m => Apply (WildEvent m e) where
  apply = ap

instance applicativeWildEvent :: MonadST s m => Applicative (WildEvent m e) where
  pure x = WildEvent $ pure $ Done x

instance monadWildEvent :: MonadST s m => Monad (WildEvent m e)
instance bindWildEvent :: MonadST s m => Bind (WildEvent m e) where
  bind (WildEvent ev) fm = WildEvent $ keepLatest $ ev <#> case _ of
    Done x -> let (WildEvent innerEv) = fm x in innerEv
    Loading -> pure Loading
    Error e -> pure $ Error e

unwrapWild :: ∀ m e a. WildEvent m e a -> AnEvent m (Wild e a)
unwrapWild (WildEvent e) = e

liftWild :: ∀ m e a. AnEvent m (Either e a) -> WildEvent m e a
liftWild ev = WildEvent $ (either Error Done) <$> ev

liftWildWithLoading
  :: ∀ s m e a. MonadST s m => Applicative m => AnEvent m (Either e a) -> WildEvent m e a
liftWildWithLoading ev = WildEvent $ ((either Error Done) <$> ev) <|> pure Loading

liftWild' :: ∀ m e a. AnEvent m a -> WildEvent m e a
liftWild' ev = WildEvent $ Done <$> ev

liftWildWithLoading' :: ∀ s m e a. MonadST s m => Applicative m => AnEvent m a -> WildEvent m e a
liftWildWithLoading' ev = WildEvent $ pure Loading <|> (Done <$> ev)

unliftLoading :: ∀ m e a. Applicative m => WildEvent m e a -> AnEvent m Unit
unliftLoading (WildEvent ev) = filterMap go ev
  where
  go Loading = Just unit
  go _ = Nothing

unliftDone :: ∀ m e a. Applicative m => WildEvent m e a -> AnEvent m a
unliftDone (WildEvent ev) = filterMap go ev
  where
  go (Done x) = Just x
  go _ = Nothing

unliftError :: ∀ m e a. Applicative m => WildEvent m e a -> AnEvent m e
unliftError (WildEvent ev) = filterMap go ev
  where
  go (Error e) = Just e
  go _ = Nothing

mapError :: ∀ m e1 e2 a. Applicative m => WildEvent m e1 a -> (e1 -> e2) -> WildEvent m e2 a
mapError (WildEvent ev) f = WildEvent $ go <$> ev
  where
  go (Error e) = Error $ f e
  go (Done x) = Done x
  go Loading = Loading