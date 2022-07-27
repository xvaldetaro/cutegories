module Platform.Misc.Disposable where

import Prelude

import Data.Either (Either, either)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)

type Disposable = Effect Unit

disposeE :: ∀ a. Either a Disposable -> Disposable
disposeE = either (const $ pure unit) identity

disposeM :: Maybe Disposable -> Disposable
disposeM m = fromMaybe (pure unit) m