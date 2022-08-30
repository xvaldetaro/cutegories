module Platform.Deku.Misc where

import Prelude

import Bolson.Core (Entity, envy)
import Bolson.Core as Bolson
import Control.Alt ((<|>))
import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal as Ref
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Deku.Attribute (Attribute)
import Deku.Control (switcher, text_)
import Deku.Core (Domable)
import Deku.DOM (Div_)
import Deku.DOM as D
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import FRP.Event (AnEvent, ZoraEvent, burning, fromEvent, keepLatest, makeEvent, subscribe, toEvent)
import Hyrule.Zora (Zora)
import Paraglider.Operator.DistinctUntilChanged (distinctUntilChangedF)
import Paraglider.Operator.DoOnNext as Paraglider
import Paraglider.Operator.DoOnSubscribe (doOnSubscribe)
import Paraglider.Operator.DoOnUnsubscribe (doOnUnsubscribe)
import Paraglider.Operator.FromAff (fromAff)
import Paraglider.Operator.Lift (lift)
import Paraglider.Operator.MemoBeh (memoBeh, memoBeh')
import Paraglider.Operator.Replay (replay, replayRefCount)
import Paraglider.Operator.ToClosure (toClosure)

type Continuation m logic obj lock a =
  (a -> Bolson.Entity logic obj m lock) -> Bolson.Entity logic obj m lock

-- wildSwitcherHappy
--   :: ∀ l p a error
--    . Show error
--   => ZoraEvent (Attribute Div_)
--   -> (ZoraEvent a -> Domable l p)
--   -> WildEvent error a
--   -> Domable l p
-- wildSwitcherHappy attrs happy = wildSwitcher attrs
--   { happy
--   , loading: const $ D.div_ [ text_ "Loading..." ]
--   , error: \e -> D.div_ [ text_ $ show e ]
--   }

-- wildSwitcher
--   :: ∀ l p a error
--    . Show error
--   => ZoraEvent (Attribute Div_)
--   -> { happy :: ZoraEvent a -> Domable l p
--      , loading :: Unit -> Domable l p
--      , error :: error -> Domable l p
--      }
--   -> WildEvent error a
--   -> Domable l p
-- wildSwitcher attrs { happy, loading, error } wildEv =
--   (distinctUntilChangedF sameWild $ unwrap wildEv) #
--     switcher D.div attrs case _ of
--       Wild.Loading -> loading unit
--       Wild.Error e -> error e
--       Wild.Happy v -> --         let
--           happyEv = unliftHappy wildEv
--         in
--           happy $ race happyEv (pure v <|> happyEv)
--   where
--   sameWild (Wild.Happy _) (Wild.Happy _) = true
--   sameWild (Wild.Loading) (Wild.Loading) = true
--   sameWild (Wild.Error _) (Wild.Error _) = true
--   sameWild _ _ = false

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