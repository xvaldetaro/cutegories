module Nuts.Game.GameNut where


import App.Env (Env)
import Deku.Control (text_)
import Deku.Core (Nut)

nut :: Env -> String -> Nut
nut env gameId = text_ "Game"