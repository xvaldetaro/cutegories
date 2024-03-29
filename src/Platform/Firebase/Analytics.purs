module Platform.Firebase.Analytics where


import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Platform.Firebase.Config (FirebaseApp)

data FirebaseAnalytics

foreign import firebaseAnalytics :: FirebaseApp -> Effect (Promise FirebaseAnalytics)

firebaseAnalyticsAff :: FirebaseApp -> Aff FirebaseAnalytics
firebaseAnalyticsAff = toAffE <<< firebaseAnalytics