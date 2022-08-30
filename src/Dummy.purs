module Dummy where

import Prelude

import Data.Int (floor)
import Effect (Effect)
import Effect.Class.Console (log)
import Platform.Firebase.Firestore.Query as Query
import Simple.JSON (unsafeStringify)


-- class Cons "id" String (tail :: Row String) (a :: Row String) <=  R' a where
--   docRef :: Record a -> String

-- instance xR' :: R' X where
--   docRef ::
-- type X = {a :: String, id :: String}

-- test :: ∀ a. R' a => Record a -> String
-- test r = docRef r

a :: Int -> Int -> String
a x y= "x:" <> show x <> show y

b :: String -> String
b s = s <> "asfd"

main :: Effect Unit
main = do
  pure unit
  -- let cs = [ Where DocId In (Multiple ["asd", "ddd"])
  --           , Where (Field "myField") Equals (Single "sss"), OrderBy (Field "field2") Asc]
  -- let a = {qtype: Collection, path: "asdfasdf/", clauses: cs}
  -- log $ unsafeStringify $ queryJsDesc a


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



