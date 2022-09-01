module Platform.Firebase.Auth
  ( authStateChangedEventWithAnonymousAccountCreation
  , firebaseAuthAff
  , User(..)
  , FirebaseAuth
  , UserMetadata
  , MultiFactorUser
  , MultiFactorInfo
  , UserInfo
  , uid
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff, Error, try)
import FRP.Event (Event)
import Foreign (Foreign)
import Paraglider.Operator.MakeEventAff (makeEventAff)
import Platform.Firebase.Config (FirebaseApp)
import Platform.Firebase.FbErr (FbErr, mapFbErr, mkErr)
import Simple.JSON as JSON

type MultiFactorInfo =
  { displayName :: Maybe String
  , enrollmentTime :: String
  , factorId :: String
  , uid :: String
  }

type MultiFactorUser =
  { enrolledFactors :: Array MultiFactorInfo
  }

type UserMetadata =
  { creationTime :: Maybe String
  , lastSignInTime :: Maybe String
  }

type UserInfo =
  { displayName :: Maybe String
  , email :: Maybe String
  , phoneNumber :: Maybe String
  , photoURL :: Maybe String
  , providerId :: String
  , uid :: String
  }

newtype User = User
  { displayName :: Maybe String
  , email :: Maybe String
  , emailVerified :: Boolean
  , isAnonymous :: Boolean
  , metadata :: Maybe UserMetadata
  , multiFactor :: Maybe MultiFactorUser
  , phoneNumber :: Maybe String
  , photoURL :: Maybe String
  , providerData :: Array UserInfo
  , providerId :: String
  , refreshToken :: String
  , tenantId :: Maybe String
  , uid :: String
  }

uid :: User -> String
uid (User {uid}) = uid

derive instance Newtype User _
derive newtype instance JSON.ReadForeign User
derive newtype instance JSON.WriteForeign User
derive newtype instance Show User


data FirebaseAuth

foreign import firebaseAuth :: FirebaseApp -> Effect (Promise FirebaseAuth)

firebaseAuthAff :: FirebaseApp -> Aff FirebaseAuth
firebaseAuthAff = toAffE <<< firebaseAuth

foreign import onAuthStateChanged
  :: (Error -> Effect Unit)
  -> (Foreign -> Effect Unit)
  -> FirebaseAuth
  -> Effect (Promise (Effect Unit))

authStateChangedEventWithAnonymousAccountCreation :: FirebaseAuth -> Event (Either FbErr User)
authStateChangedEventWithAnonymousAccountCreation auth = makeEventAff \k -> do
  let
    onAuthStateError = k <<< Left <<< mkErr "AuthStateChangedErrCb"
    onAuthStateUser = k <<< parseIncomingUser

  eiUnsub <- try $ toAffE $ onAuthStateChanged onAuthStateError onAuthStateUser auth

  let (unsub :: Effect Unit) = case eiUnsub of
        Left e -> (k $ Left $ mkErr "Auth Creation" e) *> (pure unit)
        Right unsub -> unsub

  pure unsub

parseIncomingUser :: Foreign -> Either FbErr User
parseIncomingUser fu =
  let eiJsErrUser = JSON.read fu in
  let errPrefix = show (JSON.writeJSON fu) in
  mapFbErr ("parseIncomingUser: " <> errPrefix) eiJsErrUser

-- authStateChangedEventWithAnonymousAccountCreation' :: FirebaseAuth -> Event User
-- authStateChangedEventWithAnonymousAccountCreation' auth = makeEvent \k -> do
--   unsub <- Ref.new (pure unit)
--   launchAff_ do
--     us <- toAffE $ onAuthStateChanged (show >>> Log.error)
--       ( \u -> do
--           let user' = JSON.read u
--           case user' of
--             Left e -> do
--               throwError (error $ (show (JSON.writeJSON u) <> " " <> show e))
--             Right user -> k user
--       )
--       auth
--     liftEffect $ Ref.write us unsub
--   pure $ join $ Ref.read unsub
