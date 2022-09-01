module Nuts.Room.Chatbox where

import Prelude

import App.Env (AppEvent(..))
import Control.Alt ((<|>))
import Core.Room.RoomManager (sendMessage)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (either, hush)
import Data.Formatter.DateTime (format, parseFormatString)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((:=))
import Deku.Control (text, text_)
import Deku.Core (Nut, bussed)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Doku
import Deku.Listeners (textInput)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Input (inputCss, inputText)
import Nuts.Room.RoomEnv (RoomEnv)
import Paraglider.Operator.Combine (combineLatest)
import Platform.Deku.Html (bangCss, bangCss', bangId, css, enterUp)
import Platform.Deku.Misc (dynDiffOnlyAddition, envyBurning, useMemoBeh')
import Web.DOM as DOM
import Web.DOM.Element (scrollHeight, setScrollTop)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

nut :: RoomEnv ->  Nut
nut {env, chatEv, playersEv, roomId} = Doku.do
  pushChatboxElem /\ chatboxElemEv' <- useState'
  chatboxElemEv <- envyBurning chatboxElemEv'

  let
    chatCss = bangCss'
      [ css "grow overflow-y-auto scrollbar-thin scrollbar-thumb-rounded-full"
      , css "scrollbar-track-rounded-full scrollbar-thumb-gray-900 scrollbar-track-gray-800"
      , css "first:pt-6 shadow-md px-3"
      ]

    scrollDownChat :: DOM.Element -> Effect Unit
    scrollDownChat e = do
      w <- window
      void $ w # requestAnimationFrame do
        h <- scrollHeight e
        setScrollTop h e

    onChatSelf e = pushChatboxElem e *> scrollDownChat e

    mkMessageRow playerNamesEv { ts, sender, text: msgText } =
      let playerNameEv = playerNamesEv <#> \nameDict -> fromMaybe "" $ Map.lookup sender nameDict in
      D.div (bangCss "p-2 w-full justify-between ")
        [ D.div (bangCss "flex items-baseline")
            [ D.div (bangCss "font-medium text-gray-400 mr-2") [ text playerNameEv ]
            , D.div (bangCss "text-xs font-medium text-gray-400") [ text_ $ dateText ts ]
            ]
        , D.div (bangCss "text-gray-100") [ text_ msgText ]
        ]

    typeBox :: Nut
    typeBox = bussed \pushInputVal inputValEv -> bussed \pushClear clearEv ->
      let
        pushTextGo chatboxSelf text = launchAff_ do
          eiErrorRef <- sendMessage env roomId text
          liftEffect $ either env.errPush (const $ pure unit) eiErrorRef
          liftEffect $ pushClear unit
          liftEffect $ scrollDownChat chatboxSelf
        pushMessageTextEv = combineLatest pushTextGo chatboxElemEv inputValEv
      in
        D.div (bangCss "flex mt-4 mb-6 px-3 w-full")
          [ inputText
              ( (bangCss $ inputCss <> "mr-2 grow")
                  <|> (textInput $ pure pushInputVal)
                  <|> (enterUp $ pushMessageTextEv)
                  <|> ((\_ -> D.Value := "") <$> clearEv)
              )
          , Btn.gray "Send" (css "px-8") pushMessageTextEv
          ]
    chatAttrs = chatCss <|> bangId "chatbox" <|> (pure $ D.Self := onChatSelf)

  playerNamesEv <- useMemoBeh'
    (Map.fromFoldable <<< map (\{name, id} -> Tuple id name) <$> playersEv)

  D.div (bangCss "flex flex-col h-full grow")
    [ dynDiffOnlyAddition D.div chatAttrs (mkMessageRow playerNamesEv) chatEv
    , typeBox
    ]

  where
  dateText :: Number -> String
  dateText ts = fromMaybe "" do
    formatter <- hush $ parseFormatString "MM/DD/YY - hh:m a"
    dateTime <- toDateTime <$> (instant $ Milliseconds ts)
    pure $ format formatter dateTime

