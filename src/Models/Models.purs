module Models.Models where

type PlayerId = String

type PlayerIn r = { name :: String | r}
type Player = PlayerIn ( id :: PlayerId )

type RoomId = String
type Room =
  { id :: String
  , title :: String
  , admin :: PlayerId
  , players :: Array PlayerId
  }

type ChatMessageId = String
type ChatMessageIn r = { ts :: Int, sender :: PlayerId, text :: String | r}
type ChatMessage = ChatMessageIn (id :: ChatMessageId)

type Chat = Array ChatMessage