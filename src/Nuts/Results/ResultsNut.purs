module Nuts.Game.ResultsNut where


import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except.Trans (runExceptT)
import Control.Plus (empty)
import Core.Room.GameManager (changeGameState)
import Core.Room.RoomManager (addScores)
import Core.Room.ValidationManager (observeValuation)
import Data.Array (filter, head, (:))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (switcher, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Doku
import Deku.Listeners (click)
import Effect.Aff (launchAff_)
import FRP.Event (Event, filterMap)
import Models.Models (GameState(..), ScoresConfig)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Modal (btn, dialog)
import Nuts.Results.GuessMetadataNut as GuessMetadataNut
import Nuts.Results.ScoresAggregator (PlayerWithScore, aggregateScores)
import Nuts.Results.ValidationTable (ValidationTable, mkValidationTable)
import Nuts.Room.ResultsEnv (ResultsEnv)
import Paraglider.Operator.Combine (combineLatest)
import Platform.Deku.Html (bangCss, combineCss, css, hideIf, showIf)
import Platform.Deku.Misc (cleanFbAff, ife, useCleanFbEvent, useMemoBeh')
import Platform.Firebase.Auth (uid)
import Platform.Firebase.Synonyms (FbEvent)
import Platform.Util.ErrorHandling (liftSuccess)

config :: ScoresConfig
config = {repeatedValue: 1, uniqueValue: 3}

data ConfirmAction = DiscardResults | ConfirmWinner (Array PlayerWithScore)

nut :: ResultsEnv -> Nut
nut resultsEnv@{env: env@{errPush, fb, self}, roomId, playersEv, game} = Doku.do
  pushShowConfirm /\ (showConfirmEv :: Event (Maybe ConfirmAction)) <- useState Nothing

  let (vtEv' :: FbEvent ValidationTable) = map mkValidationTable <$> observeValuation fb roomId
  vtEv :: Event ValidationTable <- useCleanFbEvent env vtEv'
  playerScoresEv <- useMemoBeh' $ aggregateScores config metaArr <$> vtEv

  let
    topScorersEv = getTopScorers <$> playerScoresEv
    topScoreEv = fromMaybe 0 <<< map (_.score) <<< head <$> playerScoresEv

    doDeclareWinners topScorers = launchAff_ $ cleanFbAff env $ runExceptT do
      liftSuccess $ addScores fb roomId ((_.id) <$> topScorers)
      liftSuccess $ changeGameState fb roomId NotStarted

    doDiscardResults = launchAff_ $ cleanFbAff env $ runExceptT do
      liftSuccess $ changeGameState fb roomId NotStarted

    renderPlayerList = switcher D.div empty renderPlayerList' playerScoresEv
    renderPlayerList' playerScores =
      D.div (bangCss "bg-gray-700 mx-1 pt-2 pb-1 mb-3 mt-3 rounded-lg flex flex-wrap") (rowUi <$> playerScores)

    rowUi player@{name, score} =
      D.li
        ( combineCss
          [ pure $ css "ml-1 mr-1 mb-1 px-2 flex font-medium text-sm rounded-full bg-gray-600 "
          , ife (css "first:font-bold first:text-white") "" <<< (_ == score) <$> topScoreEv
          ]
        )
        [ D.div (bangCss "") [text_ name]
        , D.div (bangCss "ml-1 text-teal-100 font-semibold") [text_ $ show score]
        ]

    confirmWinnerBtn = D.i
      ( (click $ topScorersEv <#> \topScorers -> pushShowConfirm $ Just $ ConfirmWinner topScorers)
          <|> bangCss ("text-teal-600 ion-checkmark text-xl" <> showIf isAdmin )
      ) []
    discardBtn = D.i
      ( (click $ pure $ pushShowConfirm $ Just DiscardResults)
          <|> bangCss ("text-red-600 ion-close-round text-xl" <> showIf isAdmin )
      ) []

    confirmationDialog = dialog $ showConfirmEv <#> map case _ of
      DiscardResults ->
        { description: "Discard results and declare NO WINNER?"
        , buttons:
          [ btn { label: "Discard", action: pure doDiscardResults, colorCss: Btn.redCss }
          , btn { label: "Cancel", action: (pure $ pushShowConfirm Nothing) }
          ]
        }
      ConfirmWinner winners ->
        { description: "Winners: " <> joinWith ", " ((_.name) <$> winners)
        , buttons:
          [ btn { label: "Confirm", action: pure $ doDeclareWinners winners, colorCss: Btn.tealCss }
          , btn { label: "Cancel", action: (pure $ pushShowConfirm Nothing) }
          ]
        }

    header = D.div (bangCss "px-3 py-2 flex items-center justify-items-stretch w-full bg-gray-700")
      [ discardBtn
      , D.span (bangCss "flex-grow w-full flex-col text-center text-lg text-white")
          [ text_ "Results" ]
      , confirmWinnerBtn
      ]

  D.div (bangCss "bg-gray-800")
    [ D.div
        (bangCss "flex flex-col max-w-4xl")
        (header : renderPlayerList : (GuessMetadataNut.nut resultsEnv vtEv <$> metaArr))
    , confirmationDialog
    ]

  where
  isAdmin = roomId == (uid self)
  metaArr = game.guessMetadataArray

  getTopScorers playerScoresArr = fromMaybe [] do
    top <- head playerScoresArr
    pure $ filter (\{score} -> score == top.score) playerScoresArr

