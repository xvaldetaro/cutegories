module Models.Player where

import Prelude

import Data.Newtype (class Newtype)
import Simple.JSON as JSON

newtype Player = Player
  { name :: String
  }

-- Derives

derive instance newtypePlayer :: Newtype Player _
derive newtype instance readForeignPlayer :: JSON.ReadForeign Player
derive newtype instance writeForeignPlayer :: JSON.WriteForeign Player
derive newtype instance eqPlayer :: Eq Player