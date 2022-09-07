module Models.Models where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Foreign (ForeignError(..), readString)
import Platform.Firebase.Firestore.DocRef (DocRef)
import Platform.Util.Similarity (Ratings)
import Prim.Row (class Lacks, class Union)
import Record as Record
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)
import Type.Proxy (Proxy(..))

type UserId = String

type PlayerId = String
type PlayerIn r = { name :: String | r }
type Player = PlayerIn (id :: UserId)
type PlayerWithRef = PlayerIn (id :: UserId, ref :: DocRef)

removeRef
  :: forall r154 a57
   . Lacks "ref" r154
  => { ref :: a57
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

type FormsPersistRow =
  ( duration :: Number
  , topic :: String
  , addRandomLetter :: Boolean
  , allowStop :: Boolean
  )
type FormsPersist = { | FormsPersistRow }

blankFormsPersist :: FormsPersist
blankFormsPersist = { duration: 60.0, topic: "", addRandomLetter: false, allowStop: false }

type ChatMessageId = String
type ChatMessageIn r = { ts :: Number, sender :: PlayerId, text :: String | r }
type ChatMessage = ChatMessageIn (id :: ChatMessageId)

type Chat = Array ChatMessage

type Guess = { userId :: UserId, text :: String }
type Guesses = { guesses :: Array Guess }

blankGuesses :: Guesses
blankGuesses = { guesses: [] }

type Game =
  { topic :: String
  , guessMetadataArray :: Array GuessMetadata
  , endsAt :: Number
  , gameState :: GameState
  , randomLetter :: Maybe String
  , id :: String
  , scoresConfig :: ScoresConfig
  , allowNonAdminToStartGame :: Boolean
  , allowStop :: Boolean
  }

blankGame :: String -> Game
blankGame id =
  { topic: ""
  , gameState: NotStarted
  , id
  , endsAt: 0.0
  , randomLetter: Nothing
  , guessMetadataArray: []
  , scoresConfig: blankScoresConfig
  , allowNonAdminToStartGame: true
  , allowStop: false
  }

type GuessMetadata = { text :: String, players :: Array Player, similars :: Ratings }
type GuessEdges = { guess :: String, edges :: Array String }
type GuessValidationRec =
  { repeated :: Array GuessEdges
  , invalid :: Array String
  }

blankValuation :: GuessValidationRec
blankValuation = { repeated: [], invalid: [] }

type ScoresConfig = { repeatedValue :: Int, uniqueValue :: Int }

blankScoresConfig :: ScoresConfig
blankScoresConfig = { repeatedValue: 1, uniqueValue: 3 }

type Bank = { id :: String,  categories :: Array String }
blankBank id = { id, categories: [] }
--

derive instance genericGameState :: Generic GameState _
instance showGameState :: Show GameState where
  show = genericShow
derive instance eqGameState :: Eq GameState

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
