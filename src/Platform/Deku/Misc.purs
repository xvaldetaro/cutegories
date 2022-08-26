module Platform.Deku.Misc where

import Prelude

import Bolson.Core (envy)
import Bolson.Core as Bolson
import Deku.Attribute (Attribute)
import Deku.Control (switcher, text_)
import Deku.Core (Domable)
import Deku.DOM (Div_)
import Deku.DOM as D
import Effect (Effect)
import Effect.Class.Console (log)
import FRP.Event (ZoraEvent, fromEvent, keepLatest, toEvent)
import Hyrule.Zora (Zora)
import Paraglider.Operator.DistinctUntilChanged (distinctUntilChangedF)
import Paraglider.Operator.DoOnNext as Paraglider
import Paraglider.Operator.DoOnSubscribe (doOnSubscribe)
import Paraglider.Operator.DoOnUnsubscribe (doOnUnsubscribe)
import Paraglider.Operator.Lift (lift)
import Paraglider.Operator.MemoBeh (memoBeh)
import Paraglider.Operator.Replay (replayRefCount)
import Platform.FRP.Wild (WildEvent(..), unwrapWild)
import Platform.FRP.Wild as Wild

type Continuation m logic obj lock a
  = (a -> Bolson.Entity logic obj m lock) -> Bolson.Entity logic obj m lock

wildSwitcher
  :: ∀ l p a error
   . Show error
  => ZoraEvent (Attribute Div_)
  -> (Unit -> Domable l p)
  -> WildEvent error a
  -> Domable l p
wildSwitcher attrs happy wildEv = (distinctUntilChangedF sameWild $ unwrapWild wildEv) #
    switcher D.div attrs case _ of
      Wild.Loading -> D.div_ [text_ "Loading..."]
      Wild.Error e -> D.div_ [text_ $ show e]
      Wild.Done _ -> happy unit
  where
  sameWild (Wild.Done _) (Wild.Done _) = true
  sameWild (Wild.Loading) (Wild.Loading) = true
  sameWild (Wild.Error _) (Wild.Error _) = true
  sameWild _ _ = false

wrapLogs :: ∀ a. String -> String -> ZoraEvent a -> ZoraEvent a
wrapLogs onSub onUnsub e =
  fromEvent $ doOnSubscribe (\_ -> log onSub) $ doOnUnsubscribe (log onUnsub) $ toEvent e

doOnNext :: ∀ a. (a -> Effect Unit) -> ZoraEvent a -> ZoraEvent a
doOnNext onNext e = fromEvent $ (Paraglider.doOnNext onNext $ (toEvent e))

shareWild :: ∀ e a logic obj lock
  . WildEvent e a
  -> (WildEvent e a -> Bolson.Entity logic obj Zora lock)
  -> Bolson.Entity logic obj Zora lock
shareWild (WildEvent e) f = envy (memoBeh e Wild.Loading (f <<< WildEvent))

shareEv :: ∀ a logic obj lock
  . ZoraEvent a
  -> (ZoraEvent a -> Bolson.Entity logic obj Zora lock)
  -> Bolson.Entity logic obj Zora lock
shareEv ev f = f $ keepLatest $ lift $ replayRefCount ev
