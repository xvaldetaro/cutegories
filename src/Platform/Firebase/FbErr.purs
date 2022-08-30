module Platform.Firebase.FbErr where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data FbErr
  = Basic String
  | DocNotFound String

derive instance genericFbErr :: Generic FbErr _
instance showFbErr :: Show FbErr where
  show = genericShow

-- mapFbErr :: ∀ a e f. Show e => Functor f => String -> f (Either e a) -> f (Either FbErr a)
-- mapFbErr at ftr = lmap (mkErr at) <$> ftr

mapFbErr :: ∀ a e. Show e => String -> Either e a -> Either FbErr a
mapFbErr at ei = lmap (mkErr at) ei

mkErr :: ∀ e. Show e => String -> e -> FbErr
mkErr at e = Basic $ "Error at " <> at <> ": " <> show e