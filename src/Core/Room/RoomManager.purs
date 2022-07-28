module Core.Room.RoomManager where

import Prelude

import App.Store.MyStore as MS
import Control.Monad.Reader (class MonadAsk)
import Halogen.Store.Monad (class MonadStore)
import Models.Models (Room(..))

-- observeRoom :: MonadStore MS.Action MS.Store m -> String -> Event (Either FSError Room)
-- roomDocEvent fs id = docEvent fs