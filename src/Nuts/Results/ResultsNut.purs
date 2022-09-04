module Nuts.Game.ResultsNut where


import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Control.Plus (empty)
import Core.Room.GameManager (changeGameState)
import Core.Room.RoomManager (addScores)
import Core.Room.ValidationManager (observeValuation)
import Data.Array (head, (:))
import Data.Maybe (Maybe(..))
import Deku.Control (switcher, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do as Doku
import Effect.Aff (launchAff_)
import FRP.Event (Event, filterMap)
import Models.Models (GameState(..), ScoresConfig)
import Nuts.Dumb.Btn as Btn
import Nuts.Results.GuessMetadataNut as GuessMetadataNut
import Nuts.Results.ScoresAggregator (aggregateScores)
import Nuts.Results.ValidationTable (ValidationTable, mkValidationTable)
import Nuts.Room.ResultsEnv (ResultsEnv)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (cleanFbAff, useCleanFbEvent, useMemoBeh')
import Platform.Firebase.Auth (uid)
import Platform.Firebase.Synonyms (FbEvent)
import Platform.Util.ErrorHandling (liftSuccess)

config :: ScoresConfig
config = {repeatedValue: 1, uniqueValue: 3}

nut :: ResultsEnv -> Nut
nut resultsEnv@{env: env@{errPush, fb, self}, roomId, playersEv, game} = Doku.do
  let (vtEv' :: FbEvent ValidationTable) = map mkValidationTable <$> observeValuation fb roomId
  vtEv :: Event ValidationTable <- useCleanFbEvent env vtEv'
  playerScoresEv <- useMemoBeh' $ aggregateScores config metaArr <$> vtEv

  let
    finishGameEv = doFinishGame <$> playerScoresEv
    doFinishGame playerScoresArr = launchAff_ $ cleanFbAff env $ runExceptT do
      let mbTopScorers = getTopScorers playerScoresArr
      liftSuccess $ addScores fb roomId mbTopScorers
      liftSuccess $ changeGameState fb roomId NotStarted

    renderHeader = switcher D.div empty renderHeader' playerScoresEv
    renderHeader' playerScores =
      D.div (bangCss "flex")
        [ Btn.teal "Done" "h-10 px-0 py-0" (finishGameEv)
        , D.div (bangCss "flex flex-col") (renderPlayerScoreRow <$> playerScores)
        ]

    renderPlayerScoreRow {id, name, score} = D.div (bangCss "flex")
      [ D.span (bangCss "text-blue-300 mr-3") [text_ name]
      , D.span (bangCss "text-white") [text_ $ show score]
      ]

  D.div (bangCss "bg-gray-800")
    [ D.div
        (bangCss "flex flex-col max-w-4xl bg-gray-700")
        (renderHeader : (GuessMetadataNut.nut resultsEnv vtEv <$> metaArr))
    ]

  where
  isAdmin = roomId == (uid self)
  metaArr = game.guessMetadataArray

  getTopScorers playerScoresArr = do
    top <- head playerScoresArr
    pure $ filterMap
      (\{score, id} -> if score == top.score then Just id else Nothing)
      playerScoresArr

