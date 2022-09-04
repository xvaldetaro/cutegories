module Core.Room.ValidationManager where

import Prelude

import App.Env (forceDocPresent)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.Array as Array
import Data.Maybe (maybe)
import Data.Set as Set
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Models.Models (RoomId, GuessValidationRec)
import Models.Paths (valuationPath)
import Nuts.Results.ValidationTable (ValidationTable, addRepeatedPair, isRepeated, repeatedEdgesToRec, rmRepeatedPair)
import Platform.FRP.FirebaseFRP (docEvent)
import Platform.Firebase.FbErr (FbErr(..))
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore.Write (updateDoc')
import Platform.Firebase.Synonyms (FbAff, FbEvent)
import Platform.Util.ErrorHandling (liftSuccess)

toggleInvalid :: FirebaseEnv -> RoomId -> String -> ValidationTable ->  FbAff Unit
toggleInvalid fb rId text {invalidSet} = updateDoc' fb.db valuationPath rId {invalid}
  where
  invalid = Array.fromFoldable $ if (Set.member text invalidSet)
    then (Set.delete text invalidSet)
    else (Set.insert text invalidSet)

togglePairRepeated :: FirebaseEnv -> RoomId -> String -> String -> ValidationTable -> FbAff Unit
togglePairRepeated fb rId t1 t2 vt =
  if isRepeated t1 t2 vt
    then makeNotRepeated fb rId vt (t1 /\ t2)
    else makeRepeated fb rId vt (t1 /\ t2)

makeRepeated :: FirebaseEnv -> RoomId -> ValidationTable -> Tuple String String -> FbAff Unit
makeRepeated fb rId vt rPair = runExceptT do
  vt' <- maybe (throwError $ Basic "makeRepeated sanity") pure (addRepeatedPair vt rPair)
  let repeated = repeatedEdgesToRec vt'.repeatedEdges
  liftSuccess $ updateDoc' fb.db valuationPath rId {repeated}

makeNotRepeated :: FirebaseEnv -> RoomId -> ValidationTable -> Tuple String String -> FbAff Unit
makeNotRepeated fb rId vt rPair = runExceptT do
  vt' <- maybe (throwError $ Basic "makeNotRepeated sanity") pure (rmRepeatedPair vt rPair)
  let repeated = repeatedEdgesToRec vt'.repeatedEdges
  liftSuccess $ updateDoc' fb.db valuationPath rId {repeated}

observeValuation :: FirebaseEnv -> RoomId -> FbEvent GuessValidationRec
observeValuation fb id = forceDocPresent <$> docEvent fb.db valuationPath id
