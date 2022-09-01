module Models.Models where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign (ForeignError(..), readString)
import Platform.Firebase.Firestore.DocRef (DocRef)
import Prim.Row (class Lacks)
import Record as Record
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)
import Type.Proxy (Proxy(..))

type UserId = String

type PlayerId = String
type PlayerIn r = { name :: String | r}
type Player = PlayerIn (id :: UserId)
type PlayerWithRef = PlayerIn (id :: UserId, ref :: DocRef)

removeRef :: forall r154 a57.
  Lacks "ref" r154 => { ref :: a57
                      | r154
                      }
                      -> Record r154
removeRef = Record.delete (Proxy :: _ "ref")

data GameState = NeverStarted | BetweenGames | LiveGame
type RoomIn r =
  { title :: String
  , gameState :: GameState
  | r
  }
type RoomId = String
type Room = RoomIn (id :: UserId)

type ChatMessageId = String
type ChatMessageIn r = { ts :: Number, sender :: PlayerId, text :: String | r}
type ChatMessage = ChatMessageIn (id :: ChatMessageId)

type Chat = Array ChatMessage

--

derive instance genericGameState :: Generic GameState _
instance showGameState :: Show GameState where
  show = genericShow
instance readForeignGameState :: ReadForeign GameState where
  readImpl fo = do
    str <- readString fo
    case str of
      "NeverStarted" -> pure NeverStarted
      "BetweenGames" -> pure BetweenGames
      "LiveGame" -> pure LiveGame
      x -> throwError $ pure $ TypeMismatch x "GameState"

instance writeForeignGameState :: WriteForeign GameState where
  writeImpl = writeImpl <<< genericShow
