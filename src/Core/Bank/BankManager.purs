module Core.Room.BankManager where

import Prelude

import App.Env (Env)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (runExceptT)
import Data.Array (findIndex, head, index, sort, union, (:))
import Data.Either (note)
import Data.Maybe (Maybe(..), fromMaybe)
import Models.Models (Bank)
import Models.Paths (bankPath)
import Platform.Firebase.Auth (uid)
import Platform.Firebase.FbErr (FbErr(..))
import Platform.Firebase.Firestore.QL (collection, whereDocIdIn)
import Platform.Firebase.Firestore.Read (getDoc, queryDocs)
import Platform.Firebase.Firestore.Write (setDoc)
import Platform.Firebase.Synonyms (FbAff)
import Platform.Util.ErrorHandling (liftSuccess)

defaultId :: String
defaultId = "defaultId"

getCombinedList :: Env -> FbAff (Array String)
getCombinedList {self, fb} = runExceptT do
  -- We get a list with the default ones for sure and perhaps the user's one
  defaultAndUserCreated <- liftSuccess $ queryDocs fb.db q

  -- The lists can come in any order so we need to make sure the user's list overwrites the default
  liftEither
    $ note (DocNotFound "no default category bank")
      $ head defaultAndUserCreated <#> \(first :: Bank) ->
        case index defaultAndUserCreated 1 of
          Nothing -> first.categories
          Just second -> if second.id == defaultId
            then union first.categories second.categories
            else union second.categories first.categories
  where
  q = collection bankPath [whereDocIdIn [defaultId, uid self]]

addCategory :: Env -> String -> FbAff Unit
addCategory {self, fb} category = runExceptT do
  let myId = defaultId -- uid self
  mbBank <- liftSuccess $ getDoc fb.db bankPath myId
  let
    bank = fromMaybe ({categories: []}) mbBank
    updatedCategories = case findIndex (_ == category) bank.categories of
      Nothing -> sort $ category : bank.categories
      Just _ -> bank.categories

  liftSuccess $ setDoc fb.db bankPath myId $ bank { categories = updatedCategories }