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

class Subset :: forall k1 k2. k1 -> k2 -> Constraint
class Subset i o
instance Union i x o => Subset (Record i) (Record o)

data GameState = NotStarted | Started | Results
type RoomIn r =
  { title :: String
  , scores :: Array UserId
  | r
  }
type RoomId = String
type Room = RoomIn (id :: UserId)

type ChatMessageId = String
type ChatMessageIn r = { ts :: Number, sender :: PlayerId, text :: String | r}
type ChatMessage = ChatMessageIn (id :: ChatMessageId)

type Chat = Array ChatMessage

type Guess = { userId :: UserId, text :: String }
type Guesses = { guesses :: Array Guess }
blankGuesses :: Guesses
blankGuesses = {guesses: []}

type Game =
  { topic :: String
  , ready :: Array String
  , endGuesses :: Array Guess
  , endsAt :: Number
  , gameState :: GameState
  , id :: String
  }
blankGame :: String -> Game
blankGame id = {topic: "", endGuesses: [], gameState: NotStarted, ready: [], id, endsAt: 0.0}

--

derive instance genericGameState :: Generic GameState _
instance showGameState :: Show GameState where
  show = genericShow
instance readForeignGameState :: ReadForeign GameState where
  readImpl fo = do
    str <- readString fo
    case str of
      "NotStarted" -> pure NotStarted
      "Started" -> pure Started
      "Results" -> pure Results
      x -> throwError $ pure $ TypeMismatch x "GameState"

instance writeForeignGameState :: WriteForeign GameState where
  writeImpl = writeImpl <<< genericShow
