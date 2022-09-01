module Models.Models where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Foreign (ForeignError(..), readString)
import Platform.Firebase.Firestore.DocRef (DocRef)
import Prim.Row (class Lacks, class Nub, class Union)
import Record as Record
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)
import Type.Proxy (Proxy(..))

type UserId = String

type PlayerId = String
type PlayerIn r = { name :: String | r }
type Player = PlayerIn (id :: UserId)
type PlayerWithRef = PlayerIn (id :: UserId, ref :: DocRef)

removeRef :: forall r154 a57.
  Lacks "ref" r154 => { ref :: a57
                      | r154
                      }
                      -> Record r154
removeRef = Record.delete (Proxy :: _ "ref")

class Subset i o
instance Union i x o => Subset (Record i) (Record o)

data GameState = NotCreated | NotStarted | Started | Results | Ended
type RoomIn r =
  { title :: String
  , scores :: Array UserId
  , gameEndSnapshot :: Maybe Game
  | r
  }
type RoomId = String
type Room = RoomIn (id :: UserId)

type ChatMessageId = String
type ChatMessageIn r = { ts :: Number, sender :: PlayerId, text :: String | r}
type ChatMessage = ChatMessageIn (id :: ChatMessageId)

type Chat = Array ChatMessage

type Guess = { userId :: UserId, text :: String }
type Game = { topic :: String, guesses :: Array Guess }

--

derive instance genericGameState :: Generic GameState _
instance showGameState :: Show GameState where
  show = genericShow
instance readForeignGameState :: ReadForeign GameState where
  readImpl fo = do
    str <- readString fo
    case str of
      "NotCreated" -> pure NotCreated
      "NotStarted" -> pure NotStarted
      "Started" -> pure Started
      "Results" -> pure Results
      "Ended" -> pure Ended
      x -> throwError $ pure $ TypeMismatch x "GameState"

instance writeForeignGameState :: WriteForeign GameState where
  writeImpl = writeImpl <<< genericShow
