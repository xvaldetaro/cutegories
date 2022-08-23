module Nuts.Game.GameNut where

import Prelude

import App.Env (Env, Nut_)
import Core.Room.RoomManager (observeRoom)
import Data.String (joinWith)
import Deku.Control (envy, text, text_)
import Deku.DOM as D
import FRP.Event (AnEvent)
import Models.Models (Room(..))
import Nuts.Room.Chatbox as Chatbox
import Paraglider.Operator.Take (take)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (shareWild, wildSwitcher)
import Platform.Deku.QualifiedDo as QualifiedDo
import Platform.FRP.Wild (unliftDone)

nut :: ∀ s m l p. Env m -> String -> Nut_ s m l p
nut env gameId = text_ "Game"