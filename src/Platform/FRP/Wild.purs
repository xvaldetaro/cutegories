module Platform.FRP.Wild where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either, either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import FRP.Event (ZoraEvent, filterMap, keepLatest)

-- | Wild abstracts data that comes "from the wild", i.e. data that needs to be loaded and can error
-- | Works like an Either with an extra "Loading" state
data Wild e a = Loading | Error e | Happy a

derive instance genericWild :: Generic (Wild e a) _
instance showWild :: (Show e, Show a) => Show (Wild e a) where
  show = genericShow

instance eqWild :: (Eq e, Eq a) => Eq (Wild e a) where
  eq = genericEq

instance ordWild :: (Ord e, Ord a) => Ord (Wild e a) where
  compare = genericCompare

derive instance functorWild :: Functor (Wild e)
instance applicativeWild :: Applicative (Wild e) where
  pure x = Happy x

instance bindWild :: Bind (Wild e) where
  bind m fm = case m of
    Happy x -> fm x
    Loading -> Loading
    Error e -> Error e

instance monadWild :: Monad (Wild e)
instance applyWild :: Apply (Wild e) where
  apply = ap

-- | An Event of Wild data.
-- | Provides helpers for chaining sequential wild operations
newtype WildEvent e a = WildEvent (ZoraEvent (Wild e a))

instance functorWildEvent :: Functor (WildEvent e) where
  map f (WildEvent ev) = WildEvent $ map (map f) ev

instance applyWildEvent :: Apply (WildEvent e) where
  apply = ap

instance applicativeWildEvent :: Applicative (WildEvent e) where
  pure x = WildEvent $ pure $ Happy x

instance monadWildEvent :: Monad (WildEvent e)
instance bindWildEvent :: Bind (WildEvent e) where
  bind (WildEvent ev) fm = WildEvent $ keepLatest $ ev <#> case _ of
    Happy x -> let (WildEvent innerEv) = fm x in innerEv
    Loading -> pure Loading
    Error e -> pure $ Error e

unwrapWild :: ∀ e a. WildEvent e a -> ZoraEvent (Wild e a)
unwrapWild (WildEvent e) = e

liftWild :: ∀ e a. ZoraEvent (Either e a) -> WildEvent e a
liftWild ev = WildEvent $ (either Error Happy) <$> ev

liftWildWithLoading :: ∀ e a. ZoraEvent (Either e a) -> WildEvent e a
liftWildWithLoading ev = WildEvent $ ((either Error Happy) <$> ev) <|> pure Loading

liftWild' :: ∀ e a. ZoraEvent a -> WildEvent e a
liftWild' ev = WildEvent $ Happy <$> ev

liftWildWithLoading' :: ∀ e a. ZoraEvent a -> WildEvent e a
liftWildWithLoading' ev = WildEvent $ pure Loading <|> (Happy <$> ev)

unliftLoading :: ∀ e a. WildEvent e a -> ZoraEvent Unit
unliftLoading (WildEvent ev) = filterMap go ev
  where
  go Loading = Just unit
  go _ = Nothing

unliftHappy :: ∀ e a. WildEvent e a -> ZoraEvent a
unliftHappy (WildEvent ev) = filterMap go ev
  where
  go (Happy x) = Just x
  go _ = Nothing

unliftError :: ∀ e a. WildEvent e a -> ZoraEvent e
unliftError (WildEvent ev) = filterMap go ev
  where
  go (Error e) = Just e
  go _ = Nothing

mapError :: ∀ e1 e2 a. WildEvent e1 a -> (e1 -> e2) -> WildEvent e2 a
mapError (WildEvent ev) f = WildEvent $ go <$> ev
  where
  go (Error e) = Error $ f e
  go (Happy x) = Happy x
  go Loading = Loading