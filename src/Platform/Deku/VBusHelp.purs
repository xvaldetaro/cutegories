module Platform.Deku.VBusHelp where

import Prelude

import Bolson.Core (envy)
import Bolson.Core as Bolson
import Control.Alt ((<|>))
import Data.Profunctor (lcmap)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\))
import Deku.Core (vbussedUncurried)
import FRP.Event (Event)
import FRP.Event.VBus (class VBus, V)

import Prim.Row as R
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

useStates'
  :: forall logic obj lock rbus bus push event
   . RowToList bus rbus
  => VBus rbus push event
  => Proxy (V bus)
  -> (({ | push } /\ { | event }) -> Bolson.Entity logic obj lock)
  -> Bolson.Entity logic obj lock
useStates' = vbussedUncurried

class InitializeEvents :: RowList Type -> Row Type -> Row Type -> Constraint
class InitializeEvents needleRL needle haystack where
  initializeEvents'
    :: Proxy needleRL -> { | needle } -> { | haystack } -> { | haystack }

instance InitializeEvents RL.Nil r1 r2 where
  initializeEvents' _ _ = identity

instance
  ( IsSymbol key
  , R.Cons key value needle' needle
  , R.Cons key (Event value) haystack' haystack
  , InitializeEvents rest needle haystack
  ) =>
  InitializeEvents (RL.Cons key value rest) needle haystack where
  initializeEvents' _ needle haystack =
    let
      key = Proxy :: _ key
    in
      initializeEvents' (Proxy :: _ rest) needle
        (Record.modify key (pure (Record.get key needle) <|> _) haystack)

initializeEvents
  :: forall needleRL needle haystack
   . RL.RowToList needle needleRL
  => InitializeEvents needleRL needle haystack
  => { | needle }
  -> { | haystack }
  -> { | haystack }
initializeEvents = initializeEvents' (Proxy :: _ needleRL)

useStates
  :: forall logic obj lock rbus bus push event needleRL
       needle
   . RowToList bus rbus
  => RowToList needle needleRL
  => InitializeEvents needleRL needle event
  => VBus rbus push event
  => Proxy (V bus)
  -> { | needle }
  -> (({ | push } /\ { | event }) -> Bolson.Entity logic obj lock)
  -> Bolson.Entity logic obj lock
useStates v needle = useStates' v <<< lcmap (map (initializeEvents needle))

vbussedFrom
  :: forall logic obj lock rbus bus push event needleRL
       needle
   . RowToList bus rbus
  => RowToList needle needleRL
  => InitializeEvents needleRL needle event
  => VBus rbus push event
  => Proxy (V bus)
  -> Event { | needle }
  -> (({ | push } /\ { | event }) -> Bolson.Entity logic obj lock)
  -> Bolson.Entity logic obj lock
vbussedFrom v eNeedle cont = envy $ (eNeedle <#> \needle -> useStates v needle cont)