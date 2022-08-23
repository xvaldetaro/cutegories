module Nuts.Landing where

import Prelude

import App.Navigation (navigate)
import App.Route as Route
import Control.Alt ((<|>))
import Data.Foldable (oneOf)
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
import Nuts.CSS.CSSSnippets (buttonCss, cardCss)
import Nuts.Dumb.Input as Input
import Platform.Deku.Html (bangCss, combineCss, css, enterUp)

type UIEvents = V
  ( editJoinRoomInput :: String
  , error :: Maybe String
  )

nut :: Nut
nut = Doku.do
  roomNamePu /\ roomNameEv <- useState ""
  errorPu /\ errorEv <- useState Nothing
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

      mainNut = D.div
        ( bangCss $ cardCss <> "flex-col flex w-96 items-center px-8" )
        [ D.div (bangCss "text-lg font-bold mb-6 mt-6 text-slate-500") [ text_ "Create a Game" ]
        , D.div (bangCss $ buttonCss <> "w-full text-center") [ text_ "Create" ]
        , break
        -- Join section
        , D.div (bangCss "text-lg font-bold mb-4 text-slate-500") [ text_ "Join a Game" ]
        , Input.nut
          { forId: "roomid"
          , labelText: pure "Game Room Id:"
          , inputAttrs: oneOf
            [ pure $ D.Placeholder := "Enter ID here..."
            , DL.textInput $ pure roomNamePu
            , enterUp $ roomNameEv <#> doJoinRoom
            ]
          }
        , D.div
            (oneOf
              [ bangCss $ buttonCss <> "w-full text-center mb-8"
              , DL.click $ roomNameEv <#> doJoinRoom
              ]
            )
            [ text_ "Enter Room" ]
        ]

  D.div_ [ errorNut, mainNut ]
  where
  break = D.div (bangCss "flex flex-row w-full items-center justify-between my-4")
    [ D.div ( bangCss "h-0 w-full border-t border-slate-300") []
    , D.div ( bangCss "text-slate-400 mx-4") [ text_ "Or" ]
    , D.div ( bangCss "h-0 w-full border-t border-slate-300") []
    ]