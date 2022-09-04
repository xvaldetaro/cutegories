module Nuts.Results.ScoresAggregator where

import Prelude

import Data.Array (length)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Models.Models (GuessMetadata, ScoresConfig, Player)
import Nuts.Results.ValidationTable (ValidationTable, hasAnyRepetition)

type PlayerWithScore = {id :: String, name :: String, score :: Int}

aggregateScores :: ScoresConfig -> Array GuessMetadata -> ValidationTable -> Array PlayerWithScore
aggregateScores config metaArr vt =
  Array.sortBy (\p1 p2 -> compare p2.score p1.score)
    $ Array.fromFoldable
      $ foldl accScoresForGuess Map.empty metaArr

  where

  accScoresForGuess :: Map String PlayerWithScore -> GuessMetadata -> Map String PlayerWithScore
  accScoresForGuess playerScoreDict {text, players} =
    if Set.member text vt.invalidSet
      then playerScoreDict
      else foldl (accPlayerScore guessScoreValue) playerScoreDict players

    where
    guessScoreValue :: Int
    guessScoreValue = if length players > 1 || hasAnyRepetition text vt
      then config.repeatedValue
      else config.uniqueValue


accPlayerScore :: Int -> Map String PlayerWithScore -> Player -> Map String PlayerWithScore
accPlayerScore scoreIncr dict {name, id} = Map.alter addOrSetScore id dict
  where
  addOrSetScore Nothing = Just { score: scoreIncr, name, id }
  addOrSetScore (Just p) = Just $ p { score = scoreIncr + p.score }
