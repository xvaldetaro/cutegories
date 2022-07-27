module Platform.Firebase.Firestore where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Profunctor (lcmap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, try)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, EffectFn6, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn6)
import Foreign (Foreign)
import Platform.Firebase.Config (FirebaseApp)
import Platform.Misc.Disposable (Disposable)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Simple.JSON as JSON

data Firestore

foreign import firestoreDb :: FirebaseApp -> Effect (Promise Firestore)

firestoreDbAff :: FirebaseApp -> Aff Firestore
firestoreDbAff = toAffE <<< firestoreDb

type DocumentReference =
  { id :: String
  , path :: String
  }

data DocumentSnapshot

foreign import removeUndefineds :: Foreign -> Foreign

foreign import addDoc_ :: EffectFn3 Firestore String Foreign (Promise DocumentReference)
foreign import getDoc_ :: EffectFn3 Firestore String String (Promise Foreign)
foreign import setDoc_ :: EffectFn4 Firestore String String Foreign (Promise Unit)
foreign import getDocs_ :: EffectFn2 Firestore String (Promise Foreign)

-- params: db path id onNext onError onCompleteEffect
-- returns Disposable effect
foreign import observeDoc_
  :: EffectFn6
       Firestore
       String
       String
       (Foreign -> Effect Unit)
       (String -> Effect Unit)
       (Effect Unit)
       Disposable

data FSError = ApiError String | JsonError String

derive instance genericFSError :: Generic FSError _
instance showFSError :: Show FSError where
  show = genericShow

setDocF :: Firestore -> String -> String -> Foreign -> Aff (Either FSError Unit)
setDocF fs path id x = do
  ei <- try $ toAffE $ (runEffectFn4 setDoc_) fs path id (removeUndefineds x)
  pure $ lmap (ApiError <<< show) ei

setDoc
  :: ∀ a
   . WriteForeign a
  => Firestore
  -> String
  -> String
  -> a
  -> Aff (Either FSError Unit)
setDoc fs path id x = setDocF fs path id $ JSON.writeImpl x

addDocF :: Firestore -> String -> Foreign -> Aff (Either FSError DocumentReference)
addDocF fs path x = do
  ei <- try $ toAffE $ (runEffectFn3 addDoc_) fs path (removeUndefineds x)
  pure $ lmap (ApiError <<< show) ei

addDoc
  :: ∀ a
   . WriteForeign a
  => Firestore
  -> String
  -> a
  -> Aff (Either FSError DocumentReference)
addDoc fs path x = addDocF fs path $ JSON.writeImpl x

getDocF :: Firestore -> String -> String -> Aff (Either FSError Foreign)
getDocF fs path id = do
  ei <- try $ toAffE $ (runEffectFn3 getDoc_) fs path id
  pure $ lmap (ApiError <<< show) ei

getDoc :: ∀ a. ReadForeign a => Firestore -> String -> String -> Aff (Either FSError a)
getDoc fs path id = parseDocResult <$> getDocF fs path id

getDocsF :: Firestore -> String -> Aff (Either FSError Foreign)
getDocsF fs path = do
  eiErrorDocs <- try $ toAffE $ (runEffectFn2 getDocs_) fs path
  pure $ lmap (ApiError <<< show) eiErrorDocs

getDocs :: ∀ a. ReadForeign a => Firestore -> String -> Aff (Either FSError (Array a))
getDocs fs path = parseDocResult <$> getDocsF fs path

observeDocF
  :: Firestore
  -> String
  -> String
  -> (Foreign -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit
  -> Effect Disposable
observeDocF fs path id onNext onError onComplete =
  (runEffectFn6 observeDoc_) fs path id onNext onError onComplete

observeDoc
  :: ∀ a
  . ReadForeign a
  => Firestore
  -> String
  -> String
  -> (Either FSError a -> Effect Unit)
  -> Effect Unit
  -> Effect Disposable
observeDoc fs path id onNext onComplete = observeDocF fs path id onNext' onError onComplete
  where
    onNext' = lcmap parseDocResult' onNext
    onError = lcmap (Left <<< ApiError) onNext

parseDocResult' :: ∀ a. ReadForeign a => Foreign -> Either FSError a
parseDocResult' doc = lmap (JsonError <<< show) (JSON.read doc)

parseDocResult :: ∀ a. ReadForeign a => Either FSError Foreign -> Either FSError a
parseDocResult eiErrorDoc = (lmap (ApiError <<< show) eiErrorDoc)
  >>= \doc -> lmap (JsonError <<< show) (JSON.read doc)
