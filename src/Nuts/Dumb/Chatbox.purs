module Nuts.Dumb.Chatbox where

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
import Deku.Core (Nut, Domable, bussed)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Doku
import Deku.Listeners (textInput)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import FRP.Event (ZoraEvent)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Input (inputCss, inputText)
import Nuts.Room.RoomEnv (RoomEnv)
import Paraglider.Operator.Combine (combineLatest, combineLatest3)
import Platform.Deku.Html (bangCss, bangCss', bangId, css, enterUp)
import Platform.Deku.Misc (dynDiffOnlyAddition, envyBurning, useMemoBeh')
import Web.DOM as DOM
import Web.DOM.Element (scrollHeight, setScrollTop)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)


type ChatboxConfig l p rowModel =
  { renderRow :: rowModel -> Domable l p
  , rowsEv :: ZoraEvent (Array rowModel)
  , sendMessageEv :: ZoraEvent (String -> Effect Unit)
  }

nut :: ∀ rm l p. ChatboxConfig l p rm -> Domable l p
nut {renderRow, rowsEv, sendMessageEv} = Doku.do
  pushChatboxElem /\ chatboxElemEv' <- useState'
  chatboxElemEv <- envyBurning chatboxElemEv'

  let
    chatCss = bangCss'
      [ css "grow overflow-y-auto scrollbar-thin scrollbar-thumb-rounded-full"
      , css "scrollbar-track-rounded-full scrollbar-thumb-gray-900 scrollbar-track-gray-800"
      , css "first:pt-6 shadow-md px-3 flex flex-col justify-end"
      ]

    scrollDownChat :: DOM.Element -> Effect Unit
    scrollDownChat e = do
      w <- window
      void $ w # requestAnimationFrame do
        h <- scrollHeight e
        setScrollTop h e

    onChatSelf e = pushChatboxElem e *> scrollDownChat e

    typeBox :: Nut
    typeBox = bussed \pushInputVal inputValEv -> bussed \pushClear clearEv ->
      let
        pushTextGo chatboxSelf text sendMessage = do
          sendMessage text
          liftEffect $ pushClear unit
          liftEffect $ scrollDownChat chatboxSelf
        pushMessageTextEv = combineLatest3 pushTextGo chatboxElemEv inputValEv sendMessageEv
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

  D.div (bangCss "flex flex-col h-full grow")
    [ dynDiffOnlyAddition D.div chatAttrs renderRow rowsEv
    , typeBox
    ]