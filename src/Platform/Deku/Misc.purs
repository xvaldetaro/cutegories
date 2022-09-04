module Platform.Deku.Misc where

import Prelude

import App.Env (Env)
import Bolson.Core (Entity, envy)
import Bolson.Core as Bolson
import Control.Alt ((<|>))
import Data.Array (drop, length, mapWithIndex)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (oneOfMap)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attribute (Attribute)
import Deku.Control (dyn)
import Deku.Core (Domable, insert, insert_, remove)
import Deku.Do (useState')
import Deku.Do as Doku
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Event (Event, filterMap, keepLatest, memoize, withLast)
import Paraglider.Operator.DiffAccum (diffAccum)
import Paraglider.Operator.DoOnSubscribe (doOnSubscribe)
import Paraglider.Operator.DoOnUnsubscribe (doOnUnsubscribe)
import Paraglider.Operator.DrainLeft (drainLeft)
import Paraglider.Operator.FromAff (fromAff)
import Paraglider.Operator.MapEffectful (mapEffectful)
import Paraglider.Operator.MemoBeh (memoBeh')
import Paraglider.Operator.Replay (replay, replayRefCount)
import Paraglider.Operator.SwitchMap (switchMap)
import Paraglider.Operator.ToClosure (toClosure)
import Platform.Firebase.FbErr (FbErr)
import Platform.Firebase.Synonyms (FbAff)

-- | Renders an Array of items with its original order. This function is used to display an Array of
-- | Models that you have no control of. Say you get all the items in your list from the backend and
-- | you just need to simply render them as they come. The items don't have any control over when
-- | they are added or removed, it all comes from upstream.
-- |
-- | Accumulates diffs between `upstr`'s emissions and insert<<<render/reorder/remove items from
-- | each diff.
dynDiffOrdered :: ∀ element lock payload a id
  . Ord id
  => (Event (Attribute element) -> Array (Domable lock payload) -> Domable lock payload)
  -> Event (Attribute element)
  -> (a -> id)
  -> (a -> Domable lock payload)
  -> Event (Array a)
  -> Domable lock payload
dynDiffOrdered elem attrs getId render upstr = dyn elem attrs dynEv
  where
  indexedUpstr :: Event (Array (Tuple Int a))
  indexedUpstr = mapWithIndex Tuple <$> upstr

  getId' (Tuple _ item) = getId item
  dynEv = keepLatest $ memoize (diffAccum getId' indexedUpstr) \diffEv ->
    let
      mkOnSelfRemovedEv selfId = diffEv # filterMap \{removed} ->
        if Map.member selfId removed then Just remove else Nothing

      -- | need to sort otherwise insertions go in wrong order

      sortedAddedEv :: Event (Array (Tuple Int a))
      sortedAddedEv = (\{added} -> Array.sortWith fst $ Array.fromFoldable added) <$> diffEv

      itemsAddedUnfoldedEv :: Event (Tuple Int a)
      itemsAddedUnfoldedEv = keepLatest $ (oneOfMap pure) <$> sortedAddedEv

      mkRow :: Tuple Int a -> Event (Bolson.Child _ _ _)
      mkRow (Tuple i item) = (pure $ insert i $ render item)
        <|> mkOnSelfRemovedEv (getId item)
    in
    mkRow <$> itemsAddedUnfoldedEv

-- | Same as dynDiffOrdered, but always adds new elements to the end of the list
dynDiffUnordered :: ∀ element lock payload a id
  . Ord id
  => (Event (Attribute element) -> Array (Domable lock payload) -> Domable lock payload)
  -> Event (Attribute element)
  -> (a -> id)
  -> (a -> Domable lock payload)
  -> Event (Array a)
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
  . (Event (Attribute element) -> Array (Domable lock payload) -> Domable lock payload)
  -> Event (Attribute element)
  -> (a -> Domable lock payload)
  -> Event (Array a)
  -> Domable lock payload
dynDiffOnlyAddition elem attrs render upstr = dyn elem attrs rowsEv
  where
  goNewItems {last, now} =
    let
      last' = fromMaybe [] last
    in
      if length last' < length now then Just (drop (length last') now) else Nothing

  newItemsDiffEv :: Event (Array a)
  newItemsDiffEv = filterMap goNewItems (withLast $ upstr)

  newItemsUnfoldedEv :: Event a
  newItemsUnfoldedEv = keepLatest $ (oneOfMap pure) <$> newItemsDiffEv

  mkRow :: a -> Event (Bolson.Child _ _ _)
  mkRow item = (pure $ insert_ $ render item)

  rowsEv :: Event (Event (Bolson.Child _ _ _))
  rowsEv = mkRow <$> newItemsUnfoldedEv

logE :: ∀ a. (a -> String) -> Event a -> Event a
logE f e = mapEffectful (\a -> (log $ f a) *> pure a) e

wrapLogs :: ∀ a. String -> String -> Event a -> Event a
wrapLogs onSub onUnsub e =
  doOnSubscribe (\_ -> log onSub) $ doOnUnsubscribe (log onUnsub) e

shareEv :: ∀ a b. Event a -> (Event a -> b) -> Event b
shareEv ev = toClosure (replayRefCount ev)

envyRefCount
  :: forall t121 t122 t124 a
  . Event a
  -> (Event a -> Entity t121 t122 t124)
  -> Entity t121 t122 t124
envyRefCount ev = envy <<< (toClosure (replayRefCount ev))

useMemoBeh' :: forall t121 t122 t124 a
  . Event a
  -> (Event a -> Entity t121 t122 t124)
  -> Entity t121 t122 t124
useMemoBeh' ev = envy <<< (memoBeh' ev)

-- | Applies MemoBeh' so that emissions from the FB event are cached and replayed for subsequent
-- | subscribers. Drains FbErr and pushes them to Env so that they are displayed
useCleanFbEvent
  :: forall a t112 t113 t115
  . Env
  -> Event (Either FbErr a)
  -> (Event a -> Entity t112 t113 t115)
  -> Entity t112 t113 t115
useCleanFbEvent { errPush } ev cont = envy $ memoBeh' ev \mEv -> envy
  (drainLeft errPush mEv \dEv -> cont dEv)

useStatefulDom :: ∀ l p a
  . ((a -> Effect Unit) -> Event a -> Domable l p)
  -> ((Domable l p /\ Event a) -> Domable l p)
  -> Domable l p
useStatefulDom getDom cont = Doku.do
  p /\ e <- useState'
  cont $ (getDom p e) /\ e

envyAffResult :: ∀ a lc o lk. Aff a -> (Event a -> Entity lc o lk) -> Entity lc o lk
envyAffResult = envyBurning <<< fromAff

envyBurning :: ∀ a lc o lk. Event a -> (Event a -> Entity lc o lk) -> Entity lc o lk
envyBurning ev = envy <<< (toClosure bev)
  where
  bev = do
    { event, connect } <- replay ev
    void $ connect
    pure event

errorEvent :: ∀ e a. Event (Either e a) -> Event e
errorEvent ev = filterMap errorGo ev
  where
  errorGo (Left e) = Just e
  errorGo _ = Nothing

-- | Applies MemoBeh' so that emissions from the FB event are cached and replayed for subsequent
-- | subscribers. Drains FbErr and pushes them to Env so that they are displayed
cleanFbAff :: Env -> FbAff Unit -> Aff Unit
cleanFbAff { errPush } aff = aff >>= (liftEffect <<< either errPush pure)

ife :: ∀ a. a -> a -> Boolean -> a
ife x y b = if b then x else y
