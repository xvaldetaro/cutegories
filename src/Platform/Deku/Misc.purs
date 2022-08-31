module Platform.Deku.Misc where

import Prelude

import Bolson.Core (Entity, envy)
import Bolson.Core as Bolson
import Control.Alt ((<|>))
import Control.Monad.ST.Class (class MonadST)
import Data.Array (drop, length, mapWithIndex)
import Data.Array as Array
import Data.Foldable (oneOfMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)
import Deku.Attribute (Attribute)
import Deku.Control (dyn)
import Deku.Core (Domable, insert, insert_, remove)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import FRP.Event (AnEvent, ZoraEvent, filterMap, fromEvent, keepLatest, mapAccum, memoize, toEvent, withLast)
import Hyrule.Zora (Zora)
import Paraglider.Operator.DiffAccum (diffAccum)
import Paraglider.Operator.DoOnNext as Paraglider
import Paraglider.Operator.DoOnSubscribe (doOnSubscribe)
import Paraglider.Operator.DoOnUnsubscribe (doOnUnsubscribe)
import Paraglider.Operator.FromAff (fromAff)
import Paraglider.Operator.MemoBeh (memoBeh')
import Paraglider.Operator.Replay (replay, replayRefCount)
import Paraglider.Operator.SwitchMap (switchMap)
import Paraglider.Operator.ToClosure (toClosure)

type Continuation m logic obj lock a =
  (a -> Bolson.Entity logic obj m lock) -> Bolson.Entity logic obj m lock
-- | Renders an Array of items with its original order. This function is used to display an Array of
-- | Models that you have no control of. Say you get all the items in your list from the backend and
-- | you just need to simply render them as they come. The items don't have any control over when
-- | they are added or removed, it all comes from upstream.
-- |
-- | Accumulates diffs between `upstr`'s emissions and insert<<<render/reorder/remove items from
-- | each diff.
dynDiffOrdered :: ∀ element lock payload a id
  . Ord id
  => (AnEvent Zora (Attribute element) -> Array (Domable lock payload) -> Domable lock payload)
  -> AnEvent Zora (Attribute element)
  -> (a -> id)
  -> (a -> Domable lock payload)
  -> AnEvent Zora (Array a)
  -> Domable lock payload
dynDiffOrdered elem attrs getId render upstr = dyn elem attrs dynEv
  where
  indexedUpstr :: ZoraEvent (Array (Tuple Int a))
  indexedUpstr = mapWithIndex Tuple <$> upstr

  getId' (Tuple _ item) = getId item
  dynEv = keepLatest $ memoize (diffAccum getId' indexedUpstr) \diffEv ->
    let
      mkOnSelfRemovedEv selfId = diffEv # filterMap \{removed} ->
        if Map.member selfId removed then Just remove else Nothing

      -- | need to sort otherwise insertions go in wrong order

      sortedAddedEv :: ZoraEvent (Array (Tuple Int a))
      sortedAddedEv = (\{added} -> Array.sortWith fst $ Array.fromFoldable added) <$> diffEv

      itemsAddedUnfoldedEv :: ZoraEvent (Tuple Int a)
      itemsAddedUnfoldedEv = keepLatest $ (oneOfMap pure) <$> sortedAddedEv

      mkRow :: Tuple Int a -> ZoraEvent (Bolson.Child _ _ Zora _)
      mkRow (Tuple i item) = (pure $ insert i $ render item)
        <|> mkOnSelfRemovedEv (getId item)
    in
    mkRow <$> itemsAddedUnfoldedEv

-- | Same as dynDiffOrdered, but always adds new elements to the end of the list
dynDiffUnordered :: ∀ element lock payload a id
  . Ord id
  => (AnEvent Zora (Attribute element) -> Array (Domable lock payload) -> Domable lock payload)
  -> AnEvent Zora (Attribute element)
  -> (a -> id)
  -> (a -> Domable lock payload)
  -> AnEvent Zora (Array a)
  -> Domable lock payload
dynDiffUnordered elem attrs getId render upstr = dyn elem attrs dynEv
  where
  dynEv = keepLatest $ memoize (diffAccum getId upstr) \diffEv ->
    let
      mkOnSelfRemovedEv selfId = diffEv # filterMap \{removed} ->
        if Map.member selfId removed then Just remove else Nothing

      itemsAddedUnfoldedEv = switchMap (\{added} -> oneOfMap pure added) diffEv

      mkRow item = (pure $ insert_  $ render item) <|> mkOnSelfRemovedEv (getId item)
    in
    mkRow <$> itemsAddedUnfoldedEv

-- | Same as dynDiffOrdered, but assumes that `upstr`'s emission is an Array where elements are only
-- | added, never removed. Because of that it doesn't use any sorting or Map
dynDiffOnlyAddition :: ∀ element lock payload a
  . (AnEvent Zora (Attribute element) -> Array (Domable lock payload) -> Domable lock payload)
  -> AnEvent Zora (Attribute element)
  -> (a -> Domable lock payload)
  -> AnEvent Zora (Array a)
  -> Domable lock payload
dynDiffOnlyAddition elem attrs render upstr = dyn elem attrs rowsEv
  where
  goNewItems {last, now} =
    let
      last' = fromMaybe [] last
    in
      if length last' < length now then Just (drop (length last') now) else Nothing

  newItemsDiffEv :: ZoraEvent (Array a)
  newItemsDiffEv = filterMap goNewItems (withLast $ upstr)

  newItemsUnfoldedEv :: ZoraEvent a
  newItemsUnfoldedEv = keepLatest $ (oneOfMap pure) <$> newItemsDiffEv

  mkRow :: a -> ZoraEvent (Bolson.Child _ _ Zora _)
  mkRow item = (pure $ insert_ $ render item)

  rowsEv :: ZoraEvent (ZoraEvent (Bolson.Child _ _ Zora _))
  rowsEv = mkRow <$> newItemsUnfoldedEv

wrapLogs :: ∀ a. String -> String -> ZoraEvent a -> ZoraEvent a
wrapLogs onSub onUnsub e =
  fromEvent $ doOnSubscribe (\_ -> log onSub) $ doOnUnsubscribe (log onUnsub) $ toEvent e

doOnNext :: ∀ a. (a -> Effect Unit) -> ZoraEvent a -> ZoraEvent a
doOnNext onNext e = fromEvent $ (Paraglider.doOnNext onNext $ (toEvent e))

shareEv :: ∀ a b. ZoraEvent a -> (ZoraEvent a -> b) -> ZoraEvent b
shareEv ev = toClosure (replayRefCount ev)

envyRefCount
  :: forall t121 t122 m t124 s126 a
   . MonadST s126 m
  => AnEvent m a
  -> (AnEvent m a -> Entity t121 t122 m t124)
  -> Entity t121 t122 m t124
envyRefCount ev = envy <<< (toClosure (replayRefCount ev))

useMemoBeh'
  :: forall t121 t122 m t124 s126 a
   . MonadST s126 m
  => AnEvent m a
  -> (AnEvent m a -> Entity t121 t122 m t124)
  -> Entity t121 t122 m t124
useMemoBeh' ev = envy <<< (memoBeh' ev)

envyAffResult
  :: forall a t112 t113 t115
   . Aff a
  -> (AnEvent Zora a -> Entity t112 t113 Zora t115)
  -> Entity t112 t113 Zora t115
envyAffResult = envyBurning <<< fromEvent <<< fromAff

envyBurning
  :: forall a s172 t191 t192 m t194
   . Bind m
  => MonadST s172 m
  => AnEvent m a
  -> (AnEvent m a -> Entity t191 t192 m t194)
  -> Entity t191 t192 m t194
envyBurning ev = envy <<< (toClosure bev)
  where
  bev = do
    { event, connect } <- replay ev
    void $ connect
    pure event