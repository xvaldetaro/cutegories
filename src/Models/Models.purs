module Models.Models where

import Platform.Firebase.Firestore.DocRef (DocRef)


type UserId = String

type PlayerId = String
type PlayerIn r = { name :: String, userId :: UserId | r}
type Player = PlayerIn (id :: PlayerId)
type PlayerWithRef = PlayerIn (ref :: DocRef)

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