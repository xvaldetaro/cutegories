module Platform.Deku.Misc where

import Prelude

import Bolson.Core (envy)
import Bolson.Core as Bolson
import Control.Alt ((<|>))
import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal as Ref
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (Attribute)
import Deku.Control (switcher, text_)
import Deku.Core (Domable)
import Deku.DOM (Div_)
import Deku.DOM as D
import Effect (Effect)
import Effect.Class.Console (log)
import FRP.Event (AnEvent, ZoraEvent, fromEvent, keepLatest, makeEvent, subscribe, toEvent)
import Hyrule.Zora (Zora)
import Paraglider.Operator.DistinctUntilChanged (distinctUntilChangedF)
import Paraglider.Operator.DoOnNext as Paraglider
import Paraglider.Operator.DoOnSubscribe (doOnSubscribe)
import Paraglider.Operator.DoOnUnsubscribe (doOnUnsubscribe)
import Paraglider.Operator.Lift (lift)
import Paraglider.Operator.MemoBeh (memoBeh)
import Paraglider.Operator.Replay (replayRefCount)
import Platform.FRP.Wild (WildEvent(..), unliftHappy, unwrapWild)
import Platform.FRP.Wild as Wild

type Continuation m logic obj lock a =
  (a -> Bolson.Entity logic obj m lock) -> Bolson.Entity logic obj m lock

wildSwitcherHappy
  :: ∀ l p a error
   . Show error
  => ZoraEvent (Attribute Div_)
  -> (ZoraEvent a -> Domable l p)
  -> WildEvent error a
  -> Domable l p
wildSwitcherHappy attrs happy = wildSwitcher attrs {happy, loading: Nothing, error: Nothing}

wildSwitcher
  :: ∀ l p a error
   . Show error
  => ZoraEvent (Attribute Div_)
  -> { happy :: ZoraEvent a -> Domable l p
     , loading :: Maybe (Unit -> Domable l p)
     , error :: Maybe (error -> Domable l p)
     }
  -> WildEvent error a
  -> Domable l p
wildSwitcher attrs {happy, loading, error} wildEv = (distinctUntilChangedF sameWild $ unwrapWild wildEv) #
  switcher D.div attrs case _ of
    Wild.Loading -> maybe (D.div_ [ text_ "Loading..." ]) (\f -> f unit) loading
    Wild.Error e -> maybe (D.div_ [ text_ $ show e ]) (\f -> f e) error
    Wild.Happy v ->
      let happyEv = unliftHappy wildEv in
      happy $ race happyEv (pure v <|> happyEv)
  where
  sameWild (Wild.Happy _) (Wild.Happy _) = true
  sameWild (Wild.Loading) (Wild.Loading) = true
  sameWild (Wild.Error _) (Wild.Error _) = true
  sameWild _ _ = false

race :: forall s m a. MonadST s m => AnEvent m a -> AnEvent m a -> AnEvent m a
race e1 e2 = makeEvent \k -> do
  r1 <- liftST $ Ref.new false
  r2 <- liftST $ Ref.new false
  u1 <- subscribe e1 \v -> do
    k v
    void $ liftST $ Ref.write true r1
  u2 <- subscribe e2 \v -> do
    used1 <- liftST $ Ref.read r1
    if used1 then (liftST $ void $ Ref.write true r2) else do
      k v
      u1
  cancel2 <- liftST $ Ref.read r2
  when cancel2 u2
  pure (u1 *> u2)

wrapLogs :: ∀ a. String -> String -> ZoraEvent a -> ZoraEvent a
wrapLogs onSub onUnsub e =
  fromEvent $ doOnSubscribe (\_ -> log onSub) $ doOnUnsubscribe (log onUnsub) $ toEvent e

doOnNext :: ∀ a. (a -> Effect Unit) -> ZoraEvent a -> ZoraEvent a
doOnNext onNext e = fromEvent $ (Paraglider.doOnNext onNext $ (toEvent e))

shareWild
  :: ∀ e a logic obj lock
   . WildEvent e a
  -> (WildEvent e a -> Bolson.Entity logic obj Zora lock)
  -> Bolson.Entity logic obj Zora lock
shareWild (WildEvent e) f = envy (memoBeh e Wild.Loading (f <<< WildEvent))

shareEv
  :: ∀ a logic obj lock
   . ZoraEvent a
  -> (ZoraEvent a -> Bolson.Entity logic obj Zora lock)
  -> Bolson.Entity logic obj Zora lock
shareEv ev f = f $ keepLatest $ lift $ replayRefCount ev
