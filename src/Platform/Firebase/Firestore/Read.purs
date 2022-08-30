module Platform.Firebase.Firestore.Read where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, try)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, EffectFn5, EffectFn7, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5, runEffectFn7)
import Foreign (Foreign)
import Platform.Firebase.Config (FirebaseApp)
import Platform.Firebase.FbErr (FbErr(..), mapFbErr, mkErr)
import Platform.Firebase.Firestore.Common (Firestore, DocumentReference)
import Platform.Firebase.Firestore.Query (Query, toJs)
import Platform.Util.ErrorHandling (liftEither', liftSuccess)
import Simple.JSON (class ReadForeign, class WriteForeign, E)
import Simple.JSON as JSON

foreign import getDoc_ :: EffectFn5 Firestore String String (∀ a. a -> Maybe a) (∀ a. Maybe a) (Promise (Maybe Foreign))

getDoc :: ∀ a. ReadForeign a => Firestore -> String -> String -> Aff (Either FbErr (Maybe a))
getDoc fs path id = runExceptT do
  mbA <- liftSuccess $ jsF
  let callsite = "getDoc:" <> path <> "/" <> id
  case mbA of
    Nothing -> pure Nothing
    Just a -> liftEither' (mkErr callsite) (JSON.read a)
  where
  jsF = do
    ei <- try $ toAffE $ (runEffectFn5 getDoc_) fs path id Just Nothing
    pure $ mapFbErr ("getDocF:" <> path <> "/" <> id) ei

foreign import queryDocs_ :: EffectFn2 Firestore Foreign (Promise Foreign)

queryDocs :: ∀ a. ReadForeign a => Firestore -> Query -> Aff (Either FbErr (Array a))
queryDocs fs query = parseDocResult ("getDocs:" <> query.path) <$> jsF
  where
  jsF = do
    eiErrorDocs <- try $ toAffE $ (runEffectFn2 queryDocs_) fs (toJs query)
    pure $ mapFbErr ("getDocsF:" <> query.path)  eiErrorDocs

-- params: db path id onNext onError onCompleteEffect
-- returns Disposable effect
foreign import observeDoc_
  :: EffectFn7
       Firestore
       String
       String
       (Foreign -> Effect Unit) -- onNext
       (String -> Effect Unit) -- onError
       (Effect Unit) -- onEmpty
       (Effect Unit) -- onComplete
       (Effect Unit)

observeDoc
  :: ∀ a
   . ReadForeign a
  => Firestore
  -> String
  -> String
  -> (Either FbErr a -> Effect Unit)
  -> Effect Unit
  -> Effect (Effect Unit)
observeDoc fs path id onNext onComplete = jsF fs path id onNext' onError onEmpty onComplete
  where
  callsite = "observeDoc:" <> path <> "/" <> id
  onNext' = JSON.read >>> mapFbErr callsite >>> onNext
  onError = mkErr callsite >>> Left >>> onNext
  onEmpty = onNext $ Left $ DocNotFound callsite
  jsF = (runEffectFn7 observeDoc_)

-- params: db path id onNext onError onCompleteEffect
-- returns Disposable effect
foreign import observeCollection_
  :: EffectFn4
      Firestore
      Foreign -- Query
      (Array Foreign -> Effect Unit) -- onNext
      (String -> Effect Unit) -- onError
      (Effect Unit)

observeQueryCollection
  :: ∀ a
   . ReadForeign a
  => Firestore
  -> Query
  -> (Either FbErr (Array a) -> Effect Unit)
  -> Effect (Effect Unit)
observeQueryCollection fs query onNext =
  jsF fs (toJs query) onNext' onError
  where
  callsite = "observeCollection:" <> query.path <> "/"
  onNext' = parseCollectionResult' >>> (mapFbErr callsite) >>> onNext
  onError = (mkErr callsite) >>> Left >>> onNext
  jsF = (runEffectFn4 observeCollection_)

parseCollectionResult' :: ∀ a. ReadForeign a => Array Foreign -> E (Array a)
parseCollectionResult' = traverse $ JSON.read

parseDocResult :: ∀ a. ReadForeign a => String -> Either FbErr Foreign -> Either FbErr a
parseDocResult callsite eiErrorDoc = eiErrorDoc >>= \doc -> mapFbErr callsite (JSON.read doc)
