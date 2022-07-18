module Platform.OpaqueSlot where

import Prelude

import Halogen as H

type OpaqueSlot slot = forall query. H.Slot query Void slot