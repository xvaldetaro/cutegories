module Dummy where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)


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
  log $ b <<< a 2 $ 3


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



