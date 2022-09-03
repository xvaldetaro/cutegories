module Platform.Firebase.Firestore.Write where

import Prelude

import Platform.Firebase.Synonyms (FbAff)
import Control.Promise (Promise, toAffE)
import Data.Either (Either)
import Effect.Aff (Aff, try)
import Effect.Uncurried (EffectFn3, EffectFn4, EffectFn5, runEffectFn3, runEffectFn4, runEffectFn5)
import Foreign (Foreign)
import Platform.Firebase.FbErr (FbErr, mapFbErr)
import Platform.Firebase.Firestore.Common (Firestore, DocumentReference)
import Platform.Firebase.Firestore.DocRef (DocRef)
import Simple.JSON (class WriteForeign)
import Simple.JSON as JSON

foreign import removeUndefineds :: Foreign -> Foreign

foreign import deleteDoc_ :: EffectFn3 Firestore String String (Promise Unit)
deleteDoc :: Firestore -> String -> String -> FbAff Unit
deleteDoc fs path id = do
  ei <- try $ toAffE $ (runEffectFn3 deleteDoc_) fs path id
  pure $ mapFbErr ("deleteDocF:" <> path <> "/" <> id) ei

foreign import setDoc_ :: EffectFn4 Firestore String String Foreign (Promise Unit)

setDocF :: Firestore -> String -> String -> Foreign -> FbAff Unit
setDocF fs path id x = do
  ei <- try $ toAffE $ (runEffectFn4 setDoc_) fs path id (removeUndefineds x)
  pure $ mapFbErr ("setDocF:" <> path <> "/" <> id) ei

setDoc
  :: ∀ a
   . WriteForeign a
  => Firestore
  -> String
  -> String
  -> a
  -> FbAff Unit
setDoc fs path id x = setDocF fs path id $ JSON.writeImpl x

foreign import addDoc_ :: EffectFn3 Firestore String Foreign (Promise DocRef)

addDocF :: Firestore -> String -> Foreign -> FbAff DocRef
addDocF fs path x = do
  ei <- try $ toAffE $ (runEffectFn3 addDoc_) fs path (removeUndefineds x)
  pure $ mapFbErr ("addDocF:" <> path) ei

addDoc
  :: ∀ a
   . WriteForeign a
  => Firestore
  -> String
  -> a
  -> FbAff DocRef
addDoc fs path x = addDocF fs path $ JSON.writeImpl x

foreign import updateDoc_
  :: EffectFn5 Firestore String String Foreign (Array ArrayUpdateF) (Promise Unit)

updateDocF
  :: Firestore
  -> String
  -> String
  -> Foreign
  -> Array ArrayUpdateF
  -> FbAff Unit
updateDocF fs path id objectFragment arrayUpdates = do
  ei <- try $ toAffE $ (runEffectFn5 updateDoc_) fs path id objectFragment arrayUpdates
  pure $ mapFbErr ("updateDocF:" <> path <> "/" <> id) ei

updateDoc
  :: ∀ a b
   . WriteForeign a
  => WriteForeign b
  => Firestore
  -> String
  -> String
  -> a
  -> Array (ArrayUpdate b)
  -> FbAff Unit
updateDoc fs path id objectFragment arrayUpdates = updateDocF fs path id
  (removeUndefineds $ JSON.writeImpl objectFragment)
  (processArrayUpdates arrayUpdates)

data ArrayOp = ArrayUnion | ArrayRemove
type ArrayUpdate a = { field :: String, op :: ArrayOp, elements :: Array a }
type ArrayUpdateF = { field :: String, op :: String, elements :: Foreign }

processArrayUpdates :: ∀ a. WriteForeign a => Array (ArrayUpdate a) -> Array (ArrayUpdateF)
processArrayUpdates updates = go <$> updates
  where
  convertOp ArrayUnion = "union"
  convertOp ArrayRemove = "remove"
  go { field, op, elements } =
    { field, op: convertOp op, elements: removeUndefineds $ JSON.writeImpl elements }