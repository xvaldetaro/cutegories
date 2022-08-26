module Models.Models where

type PlayerId = String

type PlayerIn r = { name :: String | r}
type Player = PlayerIn ( id :: PlayerId )

type RoomIn r =
  { title :: String
  , admin :: PlayerId
  , players :: Array PlayerId
  | r
  }
type RoomId = String
type Room = RoomIn (id :: RoomId)

type ChatMessageId = String
type ChatMessageIn r = { ts :: Number, sender :: PlayerId, text :: String | r}
type ChatMessage = ChatMessageIn (id :: ChatMessageId)

type Chat = Array ChatMessage