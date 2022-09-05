module Nuts.Room.JoinRoomBlock where

import Prelude

import App.Env (Env)
import App.Navigation (navigate)
import App.Route as Route
import Control.Alt ((<|>))
import Core.Room.RoomManager (addPlayerToRoom)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String (length, trim)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((:=))
import Deku.Control (switcher_, text_)
import Deku.Core (Domable)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Doku
import Deku.Listeners (click)
import Deku.Listeners as DL
import Effect.Aff (launchAff_)
import Models.Models (RoomId, Room)
import Nuts.CSS.CSSSnippets (cardCss)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Input (inputCss, inputText)
import Platform.Deku.Html (bangCss, css, enterUp)
import Platform.Deku.Misc (cleanFbAff)

nut :: ∀ l p. Env -> Room -> Domable l p
nut env room = Doku.do
  pushName /\ nameEv <- useState ""
  pushErr /\ errEv <- useState Nothing

  let
    doJoinRoom name
      | length name == 0 = pushErr (Just "Please Enter Your Name")
      | otherwise = launchAff_ $ cleanFbAff env $ addPlayerToRoom env.fb room.id myId { name }

    doJoinRoomEv = doJoinRoom <$> nameEv

    doGoToLanding = navigate Route.Landing

    header = D.div (bangCss "px-3 py-2 flex items-center justify-items-stretch w-full bg-gray-700")
      [ D.i ( (click $ pure $ doGoToLanding) <|> bangCss "ion-close-round text-xl") []
      , D.span (bangCss "flex-grow w-full flex-col text-center text-lg text-white")
          [ text_ room.title]
      , D.i (bangCss "text-gray-700 ion-forward text-xl") []
      ]

    selfPlayerNameTextInput =
      D.label (bangCss "mx-3 mt-3 font-medium")
        [ text_ "Your Nickname"
        , inputText
            ( (pure $ D.Placeholder := "Leeroy Jenkins")
                <|> (DL.textInput $ pure $ pushName <<< trim)
                  <|> (enterUp $ doJoinRoomEv)
                    <|> (bangCss $ inputCss <> css "w-full mt-1")
                      <|> (pure $ D.Maxlength := "10")
            )
        ]

    mainNut = D.div ( bangCss "flex-col flex justify-items-stretch" )
      [ header
      , selfPlayerNameTextInput
      , Btn.teal "Enter Room" "mt-6 mx-3" (doJoinRoomEv)
      ]

    errorNut e = D.div
      ( bangCss $ cardCss <> css "text-red-500 w-96 text-xl")
      [ text_ e ]

  switcher_ D.div (maybe mainNut errorNut) errEv

  where
  myId = (_.uid) (unwrap env.self)