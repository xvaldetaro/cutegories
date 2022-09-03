module Nuts.Game.ResultsNut where


import Prelude

import Bolson.Core (envy)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Core.Room.GameManager (addGuess, changeGameState, getGuessesByUser, getSelfGuesses)
import Core.Room.RoomManager (declareWinner)
import Data.Array (snoc)
import Data.Array.NonEmpty (toArray)
import Data.DateTime.Instant (unInstant)
import Data.Either (either)
import Data.Foldable (foldl)
import Data.Int (floor)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..), Seconds(..), toDuration)
import Data.Tuple.Nested ((/\))
import Deku.Control (switcher, text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Doku
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (fold, memoize)
import FRP.Event.Time (interval)
import Models.Models (GameState(..))
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Chatbox as Chatbox
import Nuts.Room.GameEnv (GameEnv)
import Nuts.Room.ResultsEnv (ResultsEnv)
import Paraglider.Operator.Combine (combineLatest)
import Paraglider.Operator.FromAff (fromAff)
import Paraglider.Operator.MapEffectful (mapEffectful)
import Paraglider.Operator.SwitchMap (switchMap)
import Paraglider.Operator.Take (take)
import Paraglider.Operator.Timeout (timeoutAt)
import Platform.Deku.Html (bangCss, css)
import Platform.Deku.Misc (useCleanFbEvent)
import Platform.Firebase.Auth (uid)

nut :: ResultsEnv -> Nut
nut {env: env@{errPush, fb, self}, roomId, playersEv} = Doku.do
  let guessesByUserSingle = switchMap (fromAff <<< getGuessesByUser fb roomId) (take 1 playersEv)
  playerGuessArrEv <- useCleanFbEvent env $ guessesByUserSingle

  let
    render playerGuessArr =
      D.div (bangCss "h-full flex justify-center bg-gray-800")
        [ D.div (bangCss "w-96 flex flex-col") (renderHeader <> (renderChild <$> playerGuessArr))
        ]

    renderHeader =
      [ Btn.red "Declare No winner!" "mb-2 mt-2 px-3" (mkDoDeclareWinnerEv Nothing)
      ]

    renderChild {userId, player: mbPlayer, guesses} =
      let name = fromMaybe ("#" <> userId) $ (_.name) <$> mbPlayer in
      D.div (bangCss "flex flex-col mt-4 border-gray-600 border-2 rounded-md rounded-t-xl items-stretch")
        ([ renderPlayerNameHeader name ] <> (renderGuess <$> toArray guesses))
      where
      renderPlayerNameHeader name = D.div
        (bangCss "flex w-full items-center justify-between bg-gray-600 border-0 rounded-md rounded-b-none")
        [ D.span (bangCss "text-lg text-blue-300 text-center ml-3 mt-2 mb-2 mr-3") [text_ name]
        , Btn.teal "Winner" (css "py-0 px-0 h-8 mr-3") (mkDoDeclareWinnerEv $ Just userId)
        ]

    renderGuess text = D.div
      (bangCss "flex mt-2 pb-2 items-center justify-between border-b border-gray-600 last:border-b-0")
      [ D.span (bangCss "ml-3") [text_ text]
      , Btn.red "Discard" (css "mr-3 py-0 px-0") (pure $ pure unit)
      ]

  envy $  render <$> playerGuessArrEv

  where
  isAdmin = roomId == (uid self)
  mkDoDeclareWinnerEv mbWinner = pure $ launchAff_ do
    eiResult <- declareWinner fb roomId mbWinner
    liftEffect $ either errPush pure eiResult
