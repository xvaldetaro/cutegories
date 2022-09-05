module Dummy where

import Prelude

import Control.Monad.Except (lift, runExceptT)
import Data.Formatter.DateTime (format, parseFormatString)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Now (nowDateTime)
import FRP.Event (Event, create, subscribe)
import Heterogeneous.Folding (hfoldlWithIndex)
import Paraglider.Operator.DiffAccum (diffAccum)
import Paraglider.Operator.Multiplex (multiplex)
import Platform.Firebase.Firestore.Query as Query
import Platform.Util.ErrorHandling (liftEither')
import Simple.JSON (unsafeStringify)
import Simple.JSON as JSON


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
  e0 <- create
  e1 <- create
  e2 <- create
  void $ subscribe (multiplex { e0: e0.event, e1: e1.event, e2: e2.event }) \e -> log $ show e
  e0.push 1
  e1.push "bar"
  e2.push "baz"
  -- let (x :: {a :: Maybe Number, b :: Maybe String}) =  {a : Nothing, b : Just "asdf"}
  -- log $ unsafeStringify $ JSON.writeImpl x
  -- multiplex { e1: pure "asdf", e2: pure 2, e3: pure {a:"a"}}
  -- f <- liftEither' (const unit) $ parseFormatString "MM/DD/YY - hh:m a"
  -- now <- lift nowDateTime
  -- lift $ log $ format f now
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



