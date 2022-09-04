module Nuts.Game.GameNut where


import Prelude

import Bolson.Core (envy)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Core.Room.GameManager (addGuess, changeGameToResults, getSelfGuesses)
import Data.Array (snoc)
import Data.DateTime.Instant (unInstant)
import Data.Either (either)
import Data.Int (floor)
import Data.Newtype (unwrap)
import Data.String (trim)
import Data.Time.Duration (Milliseconds(..), Seconds(..), toDuration)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Doku
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (fold, memoize)
import FRP.Event.Time (interval)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Chatbox as Chatbox
import Nuts.Room.GameEnv (GameEnv)
import Paraglider.Operator.FromAff (fromAff)
import Paraglider.Operator.MapEffectful (mapEffectful)
import Paraglider.Operator.SwitchMap (switchMap)
import Paraglider.Operator.Take (take)
import Paraglider.Operator.Timeout (timeoutAt)
import Paraglider.Operator.ToAff (toAff)
import Platform.Deku.Html (bangCss, css)
import Platform.Deku.Misc (cleanFbAff, useCleanFbEvent)
import Platform.Firebase.Auth (uid)

nut :: GameEnv -> Nut
nut {env: env@{errPush, fb, self}, roomId, gameEv, playersEv} = Doku.do
  lastGuessesSingle <- useCleanFbEvent env $ fromAff $ getSelfGuesses env roomId
  pushGuess /\ guessesEv' <- useState'
  let
    remainingTime {endsAt} nowInstant =
      let nowNumber = unwrap $ unInstant nowInstant in
      (toDuration $ Milliseconds (endsAt - nowNumber)) :: Seconds

    mkCountdownEv game = remainingTime game <$> interval 1000
    countdownEv = switchMap mkCountdownEv (take 1 gameEv)

    doEndGame = launchAff_ $ cleanFbAff env $ do
      players <- toAff playersEv
      changeGameToResults fb roomId players

    renderGuess text = D.div (bangCss "p-2 w-full text-gray-100") [ text_ text ]
    doPushGuess text = launchAff_ do
      liftEffect $ pushGuess text
      eiUnit <- addGuess env roomId text
      liftEffect $ either errPush pure eiUnit
    pushGuessEv = pure $ doPushGuess <<< trim
    lastTextsSingle = map (_.text) <$> lastGuessesSingle
    guessesEv = lastTextsSingle <|> (lastTextsSingle # switchMap (fold (flip snoc) guessesEv'))

    nonAdminHiddenCss = if isAdmin then "" else css "hidden mt-2"

    mkRemainingSecondsHeadline (Seconds s) = "You have " <> show (floor s)
      <> " seconds to type guesses!"

    headlineDiv =
      D.div (bangCss "px-3 mt-2 flex flex-col bg-gray-800 text-center")
        [ D.span (bangCss "text-lg font-semibold") [text_ "The Category is: "]
        , D.span (bangCss "text-2xl text-blue-300 mt-2") [text $ (_.topic) <$> gameEv ]
        , D.span (bangCss "text-lg mt-3") [text $ mkRemainingSecondsHeadline <$> countdownEv]
        ]

    timeoutEv = gameEv # switchMap \{endsAt} -> timeoutAt (Milliseconds endsAt)
    endGameEv = if isAdmin then mapEffectful (\_ -> doEndGame) timeoutEv else empty

  _ <- envy <<< (memoize endGameEv)

  D.div (bangCss "w-full h-full bg-gray-800 flex justify-center")
    [ D.div (bangCss "w-96 h-full bg-gray-700 overflow-y-auto flex flex-col")
      [ Btn.gray "End Game Early" (nonAdminHiddenCss) (pure doEndGame)
      , headlineDiv
      , Chatbox.nut {renderRow: renderGuess, rowsEv: guessesEv, sendMessageEv: pushGuessEv}
      ]
    ]

  where
  isAdmin = roomId == (uid self)