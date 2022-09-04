module Nuts.Waiting.GameDetails where

import Prelude

import Control.Alt ((<|>))
import Core.Room.GameManager (startGame)
import Data.Either (either)
import Data.Int (floor)
import Data.String (null)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Domable)
import Deku.DOM as D
import Deku.Do as Doku
import Deku.Listeners (click)
import Deku.Listeners as DL
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Input (inputText')
import Nuts.Room.RoomEnv (RoomEnv)
import Paraglider.Operator.Combine (combineLatest)
import Platform.Deku.Html (bangCss, bangPlaceholder, bangValue, combineCss, css)
import Platform.Deku.Misc (useStatefulDom)

defaultDuration :: Number
defaultDuration = 100.0

stateFulNut :: ∀ l p. RoomEnv -> (Domable l p -> Domable l p) -> Domable l p
stateFulNut { env: {fb, self, errPush}, playersEv, roomId } cont = Doku.do
  topicField /\ topicEv' <- useStatefulDom \p _ ->
    D.label (bangCss "text-lg font-medium")
      [ text_ "What Category?"
      , inputText' (bangPlaceholder "Countries In Europe" <|> (DL.textInput $ pure p))
      ]

  durationField /\ durationEv' <- useStatefulDom \p _ ->
    D.label (bangCss "text-lg font-medium")
      [ text_ "Round Duration"
      , inputText' (bangValue (show $ floor defaultDuration) <|> (DL.numeric $ pure p))
      ]

  let
    durationEv = pure defaultDuration <|> durationEv'
    topicEv = pure "" <|> topicEv'

    doCreateGame :: String -> Number -> Effect Unit
    doCreateGame topic duration = if null topic then pure unit else launchAff_ do
      eiUnit <- startGame fb roomId topic duration
      liftEffect $ either errPush pure eiUnit

    doCreateGameEv = combineLatest doCreateGame topicEv durationEv

    tooltipTextEv = topicEv <#> \topic -> if null topic
      then "Need a Category!" else "Click to Start the game"
    tooltipCssEv = topicEv <#> \topic -> if null topic
      then css "text-red-300"
      else css "text-blue-300 mt-2 text-center"
    startGameButtonCssEv = topicEv <#> \topic -> if null topic
      then css "hover:bg-red-100 bg-red-100 cursor-not-allowed"
      else ""

  cont $ D.div (bangCss "flex flex-col w-full")
    [ D.span (bangCss "text-xl text-center text-white font-semibold mt-10 mb-3") [ text_ $ "Game Details"]
    , topicField
    , durationField
    , D.div (combineCss [tooltipCssEv, pure $ css "mt-2 text-center"]) [ text tooltipTextEv]
    , D.button
        ( (click $ doCreateGameEv)
            <|> combineCss
              [ pure (Btn.baseCss <> Btn.tealCss <> css "w-full")
              , startGameButtonCssEv
              ]
        )
        [ text_ "Start Game" ]
    ]
