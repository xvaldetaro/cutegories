module Platform.Deku.Misc where

import Prelude

import Bolson.Core (Entity, envy)
import Bolson.Core as Bolson
import Control.Alt ((<|>))
import Control.Monad.ST.Class (class MonadST)
import Data.Foldable (oneOfMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Deku.Attribute (Attribute)
import Deku.Control (dyn)
import Deku.Core (Domable, insert_, remove)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import FRP.Event (AnEvent, ZoraEvent, filterMap, fromEvent, keepLatest, mapAccum, memoize, toEvent)
import Hyrule.Zora (Zora)
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


dynAccum :: ∀ element lock payload a id
  . Ord id
  => (AnEvent Zora (Attribute element) -> Array (Domable lock payload) -> Domable lock payload)
  -> AnEvent Zora (Attribute element)
  -> (a -> id)
  -> (a -> Domable lock payload)
  -> AnEvent Zora (Array a)
  -> Domable lock payload
dynAccum elem attrs getId render upstr = dyn elem attrs dynEv
  where
  dynEv = keepLatest $ memoize (diffAccum getId upstr) \diffEv ->
    let
      mkOnSelfRemovedEv selfId = diffEv # filterMap \{removed} ->
        if Map.member selfId removed then Just remove else Nothing

      itemsAddedUnfoldedEv = switchMap (\{added} -> oneOfMap pure added) diffEv

      mkRow item = (pure $ insert_  $ render item) <|> mkOnSelfRemovedEv (getId item)
    in
    mkRow <$> itemsAddedUnfoldedEv

diffAccum
  :: ∀ a id
   . Ord id
  => (a -> id)
  -> ZoraEvent (Array a)
  -> ZoraEvent { added :: Map id a, removed :: Map id a }
diffAccum getId upst = mapAccum go upst Map.empty
  where
  go incomingArr accDict =
    let
        incomingDict = Map.fromFoldable $ (\item -> Tuple (getId item) item) <$> incomingArr
        newItemsDict = Map.difference incomingDict accDict
        removedItemsDict = Map.difference accDict incomingDict
    in Tuple
      incomingDict
      { added: newItemsDict
      , removed: removedItemsDict
      }

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