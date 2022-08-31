module Models.Models where

import Platform.Firebase.Firestore.DocRef (DocRef)
import Prim.Row (class Lacks)
import Record as Record
import Type.Proxy (Proxy(..))


type UserId = String

type PlayerId = String
type PlayerIn r = { name :: String, userId :: UserId | r}
type Player = PlayerIn (id :: PlayerId)
type PlayerWithRef = PlayerIn (id :: PlayerId, ref :: DocRef)

removeRef :: forall r154 a57.
  Lacks "ref" r154 => { ref :: a57
                      | r154
                      }
                      -> Record r154
removeRef = Record.delete (Proxy :: _ "ref")

type RoomIn r =
  { title :: String
  , admin :: PlayerId
  | r
  }
type RoomId = String
type Room = RoomIn (id :: RoomId)

type ChatMessageId = String
type ChatMessageIn r = { ts :: Number, sender :: PlayerId, text :: String | r}
type ChatMessage = ChatMessageIn (id :: ChatMessageId)

type Chat = Array ChatMessage