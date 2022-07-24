module Dummy where

import Prelude

import Control.Alt ((<|>))
import Control.Promise (Promise, toAffE)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Effect (Effect)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Milliseconds(..), cancelWith, delay, joinFiber, killFiber, launchAff, launchAff_, try)
import Effect.Aff (Aff, error, launchAff_, throwError, try)
import Effect.Class (liftEffect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Ref as Ref
import FRP.Event (Event, makeEvent)
import FRP.Event (Event, makeEvent, subscribe)
import Foreign (Foreign)
import Models.Models (Player(..), PlayerInput(..))
import Platform.Firebase.Auth (FirebaseAuth)
import Platform.Firebase.Config (FirebaseApp)
import Platform.Firebase.Firebase (FirebaseEnv, startFirebase)
import Platform.Firebase.Firestore (addDoc)
import Prim.Row (class Cons)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Simple.JSON as JSON

-- class Cons "id" String (tail :: Row String) (a :: Row String) <=  R' a where
--   docRef :: Record a -> String

-- instance xR' :: R' X where
--   docRef ::
-- type X = {a :: String, id :: String}

-- test :: ∀ a. R' a => Record a -> String
-- test r = docRef r

main :: Effect Unit
main = launchAff_ do
  fb <- startFirebase
  let player = {name: "p1"}
  res <- addDoc fb.db "players" player
  log $ show res


-- type Crow = (c :: String)
-- type AaRow r = (a :: String, b :: String | r)

-- type AaRec = { | AaRow Crow }

-- test :: ∀ r. { | Crow  r } -> String
-- test x = x.c

-- main :: Effect Unit
-- main = do
--   log $ test $ { a: "asdf", b: "bsdf", c: "cdsdf" }

-- main :: Effect Unit
-- main = do
--   let ev = makeEventAff $ ma
--   unsub <- subscribe ev \s -> log ("subscribe res: " <> s)
--   launchAff_ do
--     delay $ Milliseconds 100.0
--     liftEffect unsub


-- ma :: (String -> Effect Unit) -> Aff (Effect Unit)
-- ma cb = do
--   delay $ Milliseconds 500.0
--   liftEffect $ cb "Aff cb emission"
--   pure $ log "Aff unsub"

-- main :: Effect Unit
-- main = do
--   unsub <- subscribe ev sub
--   launchAff_ do
--     delay $ Milliseconds 1000.0
--     liftEffect unsub
--   pure unit

-- sub :: String -> Effect Unit
-- sub = log

-- ev :: Event String
-- ev = makeEvent \k -> do
--     fiber <- launchAff do
--       delayAff <- delay $ Milliseconds 2000.0
--       pure $ innerUnsub :: Effect Unit <- pure $ log "innerUnsub"
--     pure $ launchAff_ do
--       log "kiling fiber"
--       killFiber (error "") fiber



