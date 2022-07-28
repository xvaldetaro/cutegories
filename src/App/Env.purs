module App.Env where

import Platform.Firebase.Firebase (FirebaseEnv)

type FirebaseEnvRow  = (fb :: FirebaseEnv )
type Env = Record (FirebaseEnvRow)