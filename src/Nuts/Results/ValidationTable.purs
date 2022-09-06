module Nuts.Results.ValidationTable where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, snoc')
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Models.Models (GuessValidationRec, GuessEdges)

type ValidationTable =
  { repeatedEdges :: Map String (Set String)
  , invalidSet :: Set String
  }

mkLeadByText :: Map String String -> NonEmptyArray String -> Map String String
mkLeadByText dict cluster = foldl addRepeats dict cluster
  where
  lead = NonEmptyArray.head cluster
  addRepeats dict' text = Map.insert text lead dict'

isInvalid :: String -> ValidationTable -> Boolean
isInvalid t {invalidSet} = Set.member t invalidSet

hasAnyRepetition :: String -> ValidationTable -> Boolean
hasAnyRepetition t {repeatedEdges} = fromMaybe false do
  edges <- Map.lookup t repeatedEdges
  pure $ not $ Set.isEmpty edges

getRepetitions :: String -> ValidationTable -> Maybe (Set String)
getRepetitions t {repeatedEdges} = Map.lookup t repeatedEdges

isRepeated :: String -> String -> ValidationTable -> Boolean
isRepeated t1 t2 {repeatedEdges} = fromMaybe false do
  t1Edges <- Map.lookup t1 repeatedEdges
  pure $ Set.member t2 t1Edges

mkValidationTable :: GuessValidationRec -> ValidationTable
mkValidationTable {repeated, invalid} = {repeatedEdges, invalidSet}
  where
  invalidSet = Set.fromFoldable invalid
  repeatedEdges = Map.fromFoldable $ mkTuple <$> repeated
  mkTuple {guess, edges} = Tuple guess (Set.fromFoldable edges)

addRepeatedPair :: ValidationTable -> Tuple String String -> Maybe ValidationTable
addRepeatedPair vt@{repeatedEdges} (t1 /\ t2) = do
  let
    t1Edges = fromMaybe Set.empty $ Map.lookup t1 repeatedEdges
    t2Edges = fromMaybe Set.empty $ Map.lookup t2 repeatedEdges
  when (Set.member t1 t2Edges || Set.member t2 t1Edges) Nothing
  let
    t1Edges' = Set.insert t2 t1Edges
    t2Edges' = Set.insert t1 t2Edges
    re' = Map.insert t2 t2Edges' $ Map.insert t1 t1Edges' repeatedEdges
  pure $ vt { repeatedEdges = re' }

rmRepeatedPair :: ValidationTable -> Tuple String String -> Maybe ValidationTable
rmRepeatedPair vt@{repeatedEdges} (t1 /\ t2) = do
  let
    t1Edges = fromMaybe Set.empty $ Map.lookup t1 repeatedEdges
    t2Edges = fromMaybe Set.empty $ Map.lookup t2 repeatedEdges
  when (not (Set.member t1 t2Edges) || not (Set.member t2 t1Edges)) Nothing
  let
    t1Edges' = Set.delete t2 t1Edges
    t2Edges' = Set.delete t1 t2Edges
    re' = Map.insert t2 t2Edges' $ Map.insert t1 t1Edges' repeatedEdges
  pure $ vt { repeatedEdges = re' }

repeatedEdgesToRec :: Map String (Set String) -> Array GuessEdges
repeatedEdgesToRec guessEdgeDict = foldlWithIndex addToArr [] guessEdgeDict
  where
  addToArr guess accArr edgesSet = Array.snoc accArr { guess, edges: Array.fromFoldable edgesSet }