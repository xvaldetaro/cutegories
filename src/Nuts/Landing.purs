module Nuts.Landing where

import Prelude

import App.Env (Env)
import App.Navigation (navigate)
import App.Route as Route
import Bolson.Control (switcher)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (runExceptT)
import Core.Room.RoomManager (addPlayerToRoom, createRoom, getRoom, getRoomForUserId)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (length)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((:=))
import Deku.Control (switcher_, text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Doku
import Deku.Listeners as DL
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (fromEvent)
import FRP.Event.VBus (V)
import Nuts.CSS.CSSSnippets (cardCss)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Input (inputCss, inputText)
import Paraglider.Operator.Combine (combineLatest)
import Paraglider.Operator.FromAff (fromAff, fromAffSafe)
import Paraglider.Operator.MemoBeh (memoBeh)
import Platform.Deku.Html (bangCss, bangId, combineCss, css, enterUp)
import Platform.Deku.Misc (envyAffResult, envyBurning)
import Platform.FRP.Led (dataEvent, errorEvent)
import Platform.Firebase.Firestore.DocRef as DocRef
import Platform.Util.ErrorHandling (liftEither', liftSuccess, liftSuccess')

type UIEvents = V
  ( editJoinRoomInput :: String
  , error :: Maybe String
  )

nut :: Env -> Nut
nut env = Doku.do
  errorPu /\ errorEv' <- useState Nothing
  roomNamePu /\ roomNameEv <- useState ""
  pushSelfPlayerName /\ selfPlayerNameEv <- useState ""
  pushNewRoomTitle /\ newRoomTitleEv <- useState ""
  let myId = (_.uid) (unwrap env.self)
  mbRoomEv <- envyAffResult $ getRoomForUserId env.fb myId

  let
    errorEv = errorEv' <|> (Just <<< show <$> errorEvent mbRoomEv)

    showError = ((const Nothing) <$> roomNameEv) <|> errorEv
    errorNut = D.div
      ( combineCss
        [ pure $ cardCss <> css "text-red-500 w-96 text-xl"
        , showError <#> maybe (css "hidden") (const $ css "block")
        ]
      )
      [ text $ fromMaybe "" <$> showError ]

    selfPlayerNameTextInput =
      D.label (bangCss "text-lg font-medium")
        [ text_ "Your Nickname"
        , inputText
            ( (pure $ D.Placeholder := "Leeroy Jenkins")
                <|> (DL.textInput $ pure pushSelfPlayerName)
                  <|> (enterUp $ doCreateRoomEv)
                    <|> (bangCss $ inputCss <> css "w-full mt-1 mb-6")
            )
        ]

    doCreateRoom roomTitle playerName
      | length playerName == 0 = errorPu (Just "Please Enter Your Name")
      | length roomTitle == 0 = errorPu (Just "Please Enter Room Title")
      | otherwise = launchAff_ do
          eiRoomRef <- createRoom env playerName roomTitle
          liftEffect $ case eiRoomRef of
            Left e -> errorPu $ Just $ show e
            Right ref -> navigate $ Route.Room (DocRef.id ref)

    doCreateRoomEv = combineLatest doCreateRoom newRoomTitleEv selfPlayerNameEv

    newRoomTitleTextInput =
      inputText
        ( (pure $ D.Placeholder := "Room Title")
            <|> (DL.textInput $ pure pushNewRoomTitle)
              <|> (enterUp $ doCreateRoomEv)
                <|> (bangCss $ inputCss <> css "w-full mb-2")
        )

    createGameBlock = Doku.do
      D.div (bangCss "w-full")
        [ newRoomTitleTextInput
        , Btn.teal "Create a Game" (css "text-lg w-full") (doCreateRoomEv)
        ]

    doJoinRoom roomId playerName
      | length playerName == 0 = errorPu (Just "Please Enter Your Name")
      | length roomId == 0 = errorPu (Just "Please Enter Room Id")
      | otherwise = launchAff_ do
        eiPlayerRef <- runExceptT do
          mbRoom <- liftSuccess' show $ getRoom env.fb roomId
          room <- liftEither' show $ note "The room doesn't Exist" mbRoom
          liftSuccess' show $ addPlayerToRoom env.fb room.id {userId: myId, name: playerName}
        liftEffect $ case eiPlayerRef of
          Left e -> errorPu $ Just e
          Right _ -> navigate $ Route.Room roomId

    doJoinRoomEv = combineLatest doJoinRoom roomNameEv selfPlayerNameEv

    joinGameTextInput =
      inputText
        ( (pure $ D.Placeholder := "Room ID #")
            <|> (DL.textInput $ pure roomNamePu)
              <|> (enterUp doJoinRoomEv)
                <|> (bangCss $ inputCss <> css "w-full mt-1")
        )

    joinGameBlock = Doku.do
      D.div (bangCss "w-full")
        [ joinGameTextInput
        , Btn.teal "Join a Game" "w-full mb-8 mt-3 text-lg" (doJoinRoomEv)
        ]

    mainNut = D.div
      ( bangCss $ cardCss <> "flex-col flex w-96 items-center px-8" )
      [ selfPlayerNameTextInput
      , break "Create"
      , createGameBlock
      , break "Join"
      , joinGameBlock
      ]

    hasRoomNut id = D.div
      (bangCss "")
      [ D.div (bangCss "text-lg") [text_ $ "You are already in room #" <> id]
      , Btn.teal "Rejoin" "" (pure $ navigate $ Route.Room id)
      ]

  mbRoomEv # switcher_ D.div case _ of
    Left e -> text_ $ "Error getting room:" <> show e
    Right mbRoomId -> case mbRoomId of
      Nothing -> D.div_ [ errorNut, mainNut ]
      Just roomId -> D.div_ [ errorNut, hasRoomNut roomId ]

  where
  break text = D.div (bangCss "flex flex-row w-full items-center justify-between my-4")
    [ D.div ( bangCss "h-0 w-full border-t border-slate-300") []
    , D.div ( bangCss "text-slate-400 mx-4") [ text_ text ]
    , D.div ( bangCss "h-0 w-full border-t border-slate-300") []
    ]