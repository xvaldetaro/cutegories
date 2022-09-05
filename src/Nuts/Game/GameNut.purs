module Nuts.Game.GameNut where


import Prelude

import Bolson.Core (envy)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Core.Room.GameManager (addGuess, changeGameToResults, getSelfGuesses)
import Core.Room.RoomManager (rmPlayerFromRoom)
import Data.Array (snoc)
import Data.DateTime.Instant (unInstant)
import Data.Either (either)
import Data.Int (floor)
import Data.Maybe (fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.String (trim)
import Data.Time.Duration (Milliseconds(..), Seconds(..), toDuration)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState, useState')
import Deku.Do as Doku
import Deku.Listeners (click)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (fold, memoize)
import FRP.Event.Time (interval)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Chatbox as Chatbox
import Nuts.Dumb.Modal (modal)
import Nuts.Room.GameEnv (GameEnv)
import Paraglider.Operator.FromAff (fromAff)
import Paraglider.Operator.MapEffectful (mapEffectful)
import Paraglider.Operator.SwitchMap (switchMap)
import Paraglider.Operator.Take (take)
import Paraglider.Operator.Timeout (timeoutAt)
import Paraglider.Operator.ToAff (toAff)
import Platform.Deku.Html (bangCss, combineCss, css, showIf)
import Platform.Deku.Misc (cleanFbAff, useCleanFbEvent)
import Platform.Firebase.Auth (uid)

nut :: GameEnv -> Nut
nut {env: env@{errPush, fb, self}, roomId, gameEv, playersEv} = Doku.do
  lastGuessesSingle <- useCleanFbEvent env $ fromAff $ getSelfGuesses env roomId
  pushGuess /\ guessesEv' <- useState'
  pushShowConfirm /\ showConfirmEv <- useState false

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

    doLeaveRoomEv = pure $ launchAff_ $ cleanFbAff env $ rmPlayerFromRoom fb roomId (uid self)

    confirmModal = modal showConfirmEv $ D.div
      (bangCss "bg-gray-200 rounded-md flex flex-col p-3 m-14")
      [ D.span (bangCss "text-gray-900 mb-3 ")
          [ text_ $ "Are you sure you want to leave this room?" ]
      , D.div (bangCss "flex justify-items-stretch w-full")
          [ Btn.red "Leave" "flex-grow mr-1" (doLeaveRoomEv)
          , Btn.gray "Cancel" "flex-grow ml-1" (pure $ pushShowConfirm false)
          ]
      ]

    leaveBtn = if isAdmin then text_ "" else D.i
      ( (click $ pure $ pushShowConfirm true)
          <|> bangCss "mt-2 ion-arrow-left-a text-xl text-red-200"
      )
      []

    randomLetterTextEv =
      D.span (combineCss [pure $ css "", showIf <<< isJust <<< (_.randomLetter) <$> gameEv ])
        [ text_ "Starting with letter: "
        , D.span (bangCss "font-bold text-lg text-blue-200") [text $ fromMaybe "" <<< (_.randomLetter) <$> gameEv]
        ]

    headlineDiv =
      D.div (bangCss "px-3 mt-2 flex flex-col text-center flex-grow")
        [ D.span (bangCss "text-lg text-blue-200") [text $ (_.topic) <$> gameEv ]
        , randomLetterTextEv
        ]

    timeoutEv = gameEv # switchMap \{endsAt} -> timeoutAt (Milliseconds endsAt)
    endGameEv = if isAdmin then mapEffectful (\_ -> doEndGame) timeoutEv else empty

  _ <- envy <<< (memoize endGameEv)

  D.div (bangCss "flex flex-col w-full h-full items-stretch bg-gray-800")
    [ confirmModal
    , D.div (bangCss "px-3 py-2 flex items-center justify-items-stretch w-full bg-gray-700")
        [ leaveBtn
        , headlineDiv
        , D.i (bangCss "text-gray-700 ion-forward text-xl") []
        ]
    , D.span (bangCss "text-lg mt-3") [text $ mkRemainingSecondsHeadline <$> countdownEv]
    , Btn.gray "End Game Early" (nonAdminHiddenCss) (pure doEndGame)
    , Chatbox.nut {renderRow: renderGuess, rowsEv: guessesEv, sendMessageEv: pushGuessEv}
    ]

  where
  isAdmin = roomId == (uid self)