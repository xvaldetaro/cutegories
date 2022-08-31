module Nuts.Debug where


import Prelude

import App.Env (Env)
import Bolson.Core as BCore
import Control.Alt ((<|>))
import Control.Plus (empty)
import Core.Room.RoomManager (addPlayerToRoom, getPlayerForUser, observeChat, observeRoom, observeRoomPlayers, rmPlayerFromRoom)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Deku.Control (dyn_, text, text_)
import Deku.Core (Domable, Node, insert_, remove)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Doku
import FRP.Event (ZoraEvent, delay, filterMap, fromEvent)
import Hyrule.Zora (Zora)
import Models.Models (Chat, Player)
import Nuts.Dumb.Btn as Btn
import Paraglider.Operator.FromAff (fromAff)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (wrapLogs)
import Platform.FRP.FirebaseFRP (collectionEvent)
import Platform.FRP.Led (loadingEvent)
import Platform.FRP.Wild (WildEvent, unliftHappy)
import Platform.Firebase.FbErr (FbErr)
import Record as Record
import Type.Proxy (Proxy(..))

nut :: ∀ l p. Env -> Domable l p
nut env = Doku.do
  let
    myId = (_.uid) (unwrap env.self)
    mbRoomEv = fromEvent $ fromAff $ getPlayerForUser env.fb myId

  text $ show <<< map (map (Record.delete (Proxy :: _ "ref"))) <$> mbRoomEv
  --   aDiv = text_ "aDiv"
  --   bDiv = text_ "bDiv"
  --   bF boolean = if boolean
  --     then
  --       (pure $ insert_ aDiv)
  --         <|> (filterMap (\boolean' -> if boolean' then Nothing else Just remove) aEv)
  --     else
  --       (pure $ insert_ bDiv)

  --   dynEvent = aEv <#> bF

  -- dyn_ D.div dynEvent
  -- ps /\ sev <- useState'
  -- let roomId = "mvSmbaF0wDC5DhcG5Ic0"
  -- wildRoom <- envyMemoWild $ observeRoom fb roomId
  -- wildPlayers <- envyMemoWild $ observeRoomPlayers fb roomId
  -- wildChat <- envyMemoWild $ observeChat fb roomId
  -- let
  --   allLoaded = wildRoom *> wildChat
  --   happy _ = D.div_ [text (show <$> (unliftHappy wildRoom))]
  --     -- D.div_
  --     --   [ text $ show <$> (unliftHappy wildChat)
  --     --   , text $ show <$> (unwrap wildPlayers)
  --     --   , text $ show <$> (unliftHappy wildRoom)
  --     --   ]

  -- allLoaded # wildSwitcher (bangCss "h-full") {happy, loading, error}
  -- -- happy unit
  -- where
  -- loading _ = D.div_ [text_ "Loading..."]
  -- error e = D.div_ [text_ $ "Error: " <> show e]
