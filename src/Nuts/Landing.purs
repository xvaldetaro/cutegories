module Nuts.Landing where

import Prelude

import App.Navigation (navigate)
import App.Route as Route
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (length)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((:=))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Doku
import Deku.Listeners as DL
import FRP.Event.VBus (V)
import Nuts.CSS.CSSSnippets (cardCss)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Input (inputCss, inputText)
import Platform.Deku.Html (bangCss, combineCss, css, enterUp)

type UIEvents = V
  ( editJoinRoomInput :: String
  , error :: Maybe String
  )

nut :: Nut
nut = Doku.do
  errorPu /\ errorEv <- useState Nothing
  roomNamePu /\ roomNameEv <- useState ""

  let
      showError = ((const Nothing) <$> roomNameEv) <|> errorEv
      errorNut = D.div
        ( combineCss
          [ pure $ cardCss <> css "text-red-700 w-96"
          , showError <#> maybe (css "hidden") (const $ css "block")
          ]
        )
        [ text $ fromMaybe "" <$> showError ]

      doJoinRoom roomId = do
        if (length roomId == 0) then errorPu (Just "Please Enter Room Id")
        else navigate $ Route.Room roomId

      createGameBlock = Doku.do
        D.div (bangCss "w-full")
          [ Btn.teal "Create a Game" (css "text-lg w-full") (pure $ pure unit)
          ]

      joinGameTextInput =
        inputText
          ( (pure $ D.Placeholder := "Room ID #")
              <|> (DL.textInput $ pure roomNamePu)
                <|> (enterUp $ roomNameEv <#> doJoinRoom)
                  <|> (bangCss $ inputCss <> css "w-full mt-1")
          )

      joinGameBlock = Doku.do
        D.div (bangCss "w-full")
          [ joinGameTextInput
          , Btn.teal "Join a Game" "w-full mb-8 mt-3 text-lg" (roomNameEv <#> doJoinRoom)
          ]

      mainNut = D.div
        ( bangCss $ cardCss <> "flex-col flex w-96 items-center px-8" )
        [ createGameBlock
        , break
        , joinGameBlock
        ]

  D.div_ [ errorNut, mainNut ]
  where
  break = D.div (bangCss "flex flex-row w-full items-center justify-between my-4")
    [ D.div ( bangCss "h-0 w-full border-t border-slate-300") []
    , D.div ( bangCss "text-slate-400 mx-4") [ text_ "Or" ]
    , D.div ( bangCss "h-0 w-full border-t border-slate-300") []
    ]