module Platform.Deku.VBusHelp where

import Prelude

import Bolson.Core (envy)
import Bolson.Core as Bolson
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\))
import Deku.Do (class InitializeEvents, useStates)
import FRP.Event (Event)
import FRP.Event.VBus (class VBus, V)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList)
import Record as Record
import Type.Proxy (Proxy)

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