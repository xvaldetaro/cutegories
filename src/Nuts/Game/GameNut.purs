module Nuts.Game.GameNut where

import Prelude

import Bolson.Core (envy)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Core.Room.GameManager (changeGameToResults)
import Core.Room.RoomManager (rmPlayerFromRoom)
import Data.DateTime.Instant (unInstant)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..), Seconds(..), toDuration)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Doku
import Deku.Listeners (click)
import Effect.Aff (launchAff_)
import FRP.Behavior (sample)
import FRP.Behavior.Time as Behavior
import FRP.Event (memoize)
import FRP.Event.Time (interval)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Modal (btn, dialog)
import Nuts.Game.GameGuessBox as GameGuessBox
import Nuts.Game.TimerBar as TimerBar
import Nuts.Room.GameEnv (GameEnv)
import Paraglider.Operator.Combine (combineLatest)
import Paraglider.Operator.MapEffectful (mapEffectful)
import Paraglider.Operator.Take (take)
import Paraglider.Operator.Timeout (timeoutAt)
import Paraglider.Operator.ToAff (toAff)
import Platform.Deku.Html (bangCss, css, showIf)
import Platform.Deku.Misc (cleanFbAff)
import Platform.Firebase.Auth (uid)

nut :: GameEnv -> Nut
nut gameEnv@{ env: env@{ fb, self }, game, roomId, playersEv } = Doku.do
  pushShowConfirm /\ showConfirmEv <- useState true

  let
    remainingTime { endsAt } nowInstant =
      let
        nowNumber = unwrap $ unInstant nowInstant
      in
        (toDuration $ Milliseconds (endsAt - nowNumber)) :: Seconds

    countdownEv = (remainingTime game <$> interval 1000)
      <|> (sample Behavior.instant (pure $ remainingTime game))

    progressEv = combineLatest
      (\(Seconds s) (Seconds dur) -> ceil $ 100.0 * s / dur)
      countdownEv
      (take 1 countdownEv)

    doEndGame = launchAff_ $ cleanFbAff env $ do
      players <- toAff playersEv
      changeGameToResults fb roomId players

    nonAdminHiddenCss = if isAdmin then "mx-3 mb-4" else css "hidden"

    doLeaveRoomEv = pure $ launchAff_ $ cleanFbAff env $ rmPlayerFromRoom fb roomId (uid self)

    confirmLeaveDialog = dialog $ showConfirmEv <#>
      if _ then Nothing
      else Just
        { description: "Are you sure you want to leave this room?"
        , buttons:
            [ btn { label: "Leave", action: doLeaveRoomEv, colorCss: Btn.redCss }
            , btn { label: "Cancel", action: (pure $ pushShowConfirm false) }
            ]
        }

    leaveBtn =
      if isAdmin then text_ ""
      else D.i
        ( (click $ pure $ pushShowConfirm true)
            <|> bangCss "mt-2 ion-arrow-left-a text-xl text-red-200"
        )
        []

    randomLetterTextEv =
      D.span (bangCss $ showIf <<< isJust <<< (_.randomLetter) $ game )
        [ text_ "Starting with letter: "
        , D.span (bangCss "font-bold text-lg text-blue-200")
            [ text_ $ fromMaybe "" <<< (_.randomLetter) $ game ]
        ]

    headlineDiv =
      D.div (bangCss "px-3 mt-2 flex flex-col text-center flex-grow")
        [ D.span (bangCss "text-lg text-blue-200") [ text_ $ (_.topic) $ game ]
        , randomLetterTextEv
        ]

    timeoutEv = timeoutAt (Milliseconds game.endsAt)
    endGameEv = if isAdmin then mapEffectful (\_ -> doEndGame) timeoutEv else empty

  _ <- envy <<< (memoize endGameEv)

  D.div (bangCss "flex flex-col w-full h-full items-stretch bg-gray-800")
    [ confirmLeaveDialog
    , D.div (bangCss "mb-4 px-3 py-2 flex items-center justify-items-stretch w-full bg-gray-700")
        [ leaveBtn
        , headlineDiv
        , D.i (bangCss "text-gray-700 ion-forward text-xl") []
        ]
    , D.div (bangCss "mb-4") [ TimerBar.nut progressEv countdownEv ]
    , Btn.gray "End Game Early" (nonAdminHiddenCss) (pure doEndGame)
    , D.div (bangCss "flex-grow rounded-t-xl bg-gray-700") [ GameGuessBox.nut gameEnv ]
    ]

  where
  isAdmin = roomId == (uid self)