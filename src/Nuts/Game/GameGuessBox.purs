module Nuts.Game.GameGuessBox where

import Prelude

import Control.Alt ((<|>))
import Core.Room.GameManager (addGuess, getSelfGuesses)
import Data.Array (snoc)
import Data.Either (either)
import Data.String (trim)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Doku
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (fold)
import Nuts.Dumb.Chatbox as Chatbox
import Nuts.Room.GameEnv (GameEnv)
import Paraglider.Operator.FromAff (fromAff)
import Paraglider.Operator.SwitchMap (switchMap)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (useCleanFbEvent)

nut :: GameEnv -> Nut
nut {env: env@{errPush}, roomId } = Doku.do
  lastGuessesSingle <- useCleanFbEvent env $ fromAff $ getSelfGuesses env roomId
  pushGuess /\ guessesEv' <- useState'

  let
    renderGuess text = D.div (bangCss "pl-2 flex items-center first:mt-auto")
      [ D.i (bangCss "ion-arrow-right-c") []
      , D.div (bangCss "px-3 py-1 w-full text-white") [ text_ text ]
      ]
    doPushGuess text = launchAff_ do
      liftEffect $ pushGuess text
      eiUnit <- addGuess env roomId text
      liftEffect $ either errPush pure eiUnit
    pushGuessEv = pure $ doPushGuess <<< trim
    lastTextsSingle = map (_.text) <$> lastGuessesSingle
    guessesEv = lastTextsSingle <|> (lastTextsSingle # switchMap (fold (flip snoc) guessesEv'))

  Chatbox.nut {renderRow: renderGuess, rowsEv: guessesEv, sendMessageEv: pushGuessEv}