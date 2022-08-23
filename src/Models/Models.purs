module Models.Models where

import Prelude

import Data.Newtype (class Newtype)
import Simple.JSON as JSON

type PlayerId = String

type PlayerIn r = { name :: String | r}
newtype Player = Player (PlayerIn ( id :: PlayerId ))
newtype PlayerInput = PlayerInput { name :: String }

newtype Room = Room
  { id :: String
  , title :: String
  , admin :: PlayerId
  , players :: Array PlayerId
  , chatId :: String
  }

newtype ChatMessage = ChatMessage
  { timestamp :: Int, playerId :: PlayerId, text :: String }

newtype Chat = Chat { id :: String, messages :: Array ChatMessage }

-- Derives
derive instance newtypePlayer :: Newtype Player _
derive newtype instance showPlayer :: Show Player
derive newtype instance readForeignPlayer :: JSON.ReadForeign Player
derive newtype instance writeForeignPlayer :: JSON.WriteForeign Player
derive newtype instance eqPlayer :: Eq Player
derive newtype instance showRoom :: Show Room

derive instance newtypeRoom :: Newtype Room _
derive newtype instance readForeignRoom :: JSON.ReadForeign Room
derive newtype instance writeForeignRoom :: JSON.WriteForeign Room
derive newtype instance eqRoom :: Eq Room

derive instance newtypeChatMessage :: Newtype ChatMessage _
derive newtype instance readForeignChatMessage :: JSON.ReadForeign ChatMessage
derive newtype instance writeForeignChatMessage :: JSON.WriteForeign ChatMessage
derive newtype instance eqChatMessage :: Eq ChatMessage
derive newtype instance showChat :: Show Chat
derive newtype instance showChatMessage :: Show ChatMessage


derive instance newtypeChat :: Newtype Chat _
derive newtype instance readForeignChat :: JSON.ReadForeign Chat
derive newtype instance writeForeignChat :: JSON.WriteForeign Chat
derive newtype instance eqChat :: Eq Chat
