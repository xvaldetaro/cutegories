module Nuts.Waiting.WaitingPlayerList where

import Prelude

import Control.Alt ((<|>))
import Core.Room.RoomManager (rmPlayerFromRoom)
import Data.Array as Array
import Data.Filterable (filterMap)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Doku
import Deku.Listeners (click)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (Event)
import Models.Models (Player)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Modal (modal)
import Nuts.Results.ScoresAggregator (PlayerWithScore)
import Nuts.Room.RoomEnv (RoomEnv)
import Paraglider.Operator.Combine (combineLatest)
import Platform.Deku.Html (bangCss, combineCss, css, hideIf, showIf)
import Platform.Deku.Misc (cleanFbAff, dynDiffOrdered, ife)

nut :: RoomEnv -> Nut
nut { env: env@{fb, self}, roomId, roomEv, playersEv} = Doku.do
  pushMbKick /\ (mbKickEv :: Event (Maybe PlayerWithScore)) <- useState Nothing

  let
    kickPlayer {id} = launchAff_ do
      cleanFbAff env $ rmPlayerFromRoom fb roomId id
      liftEffect $ pushMbKick Nothing

    kickPlayerEv = mbKickEv # filterMap \mbPlayer -> kickPlayer <$> mbPlayer

    kickableNameEv :: Event String
    kickableNameEv = mbKickEv # filterMap \mbPlayer -> ((_.name) <$> mbPlayer)

    kickModal = modal (isJust <$> mbKickEv) $ D.div
      (bangCss "bg-gray-200 rounded-md flex flex-col p-3 m-14")
      [ D.span (bangCss "text-gray-900 mb-3 ")
          [ text $ (\name -> "Kick " <> name <> "?") <$>  kickableNameEv]
      , D.div (bangCss "flex justify-items-stretch w-full")
          [ Btn.red "Kick" "flex-grow mr-1" kickPlayerEv
          , Btn.gray "Cancel" "flex-grow ml-1" (pure $ pushMbKick Nothing)
          ]
      ]

    rowUi player@{name, score} =
      D.li (bangCss "ml-1 mr-1 mb-1 px-2 flex font-medium text-sm first:font-bold first:text-white rounded-full bg-gray-600 ")
        [ kickButton player
        , D.div (bangCss "") [text_ name]
        , D.div (bangCss "ml-1 text-teal-100 font-semibold") [text_ $ show score]
        ]

    kickButton player@{id} = D.i
      ( (bangCss $ "ion-minus-circled text-red-500 mr-1" <> hideIf (myId == id || myId /= roomId))
          <|> (click $ pure $ pushMbKick $ Just player)
      ) []

  D.div (bangCss "bg-gray-700 flex items-start rounded-md mt-1 pt-1 mx-1")
    [ kickModal
    , dynDiffOrdered D.ul (bangCss "flex flex-wrap") (_.id) rowUi playersEvMeFirst
    ]

  where
  processPlayers playerArr scoresDict =
    let
      withScores = playerArr <#> \{name, id} ->
        {name, id, score: (fromMaybe 0 $ Map.lookup id scoresDict) }
      sorted = withScores # Array.sortBy \p1 p2 -> compare p2.score p1.score
    in
    fromMaybe sorted do
      myIndex <- Array.findIndex (\{id} -> id == myId) sorted
      mePlayer <- Array.index sorted myIndex
      arrayWithoutMe <- Array.deleteAt myIndex sorted
      pure $ Array.cons mePlayer arrayWithoutMe

  scoresDictEv = roomEv <#> \{scores} -> Map.fromFoldableWith (+) (scores <#> \id -> Tuple id 1)

  playersEvMeFirst = combineLatest processPlayers playersEv scoresDictEv

  myId = (_.uid) $ unwrap self