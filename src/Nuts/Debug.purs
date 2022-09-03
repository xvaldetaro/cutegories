module Nuts.Debug where


import Prelude

import App.Env (Env)
import Bolson.Core (envy)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Core.Room.RoomManager (deleteRoom)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.Tuple.Nested ((/\))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Doku
import Deku.Listeners (click, textInput)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Input (inputCss)
import Paraglider.Operator.MemoBeh (memoBeh)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (useStatefulDom)

nut :: ∀ l p. Env -> Domable l p
nut env = Doku.do
  inputDom /\ textEv <- useStatefulDom \p e -> D.input (bangCss inputCss <|> (textInput $ pure p)) []
  let
    doDeleteRoom id = launchAff_ do
      x <- deleteRoom env.fb (trim id)
      log $ show x
  D.div (bangCss "flex")
    [ inputDom
    , Btn.red "Delete Room" "" (doDeleteRoom <$> textEv)
    ]
  -- p /\ e' <- useState'
  -- e <- envy <<< memoBeh e' Nothing

  -- e # switcher D.div (empty) case _ of
  --   Nothing -> D.div_
  --     [ text_ "Nothing"
  --     , D.button  (click $ pure $ p $ Just 0) [text_ "To Just 0"]
  --     ]
  --   Just _ -> D.div_
  --     [ D.button  (click $ pure $ p Nothing) [text_ "To Nothing"]
  --     , D.button  (click (e <#> \v -> p ((_ + 1) <$> v))) [text_ "Increment"]
  --     , text $ show <$> e
  --     ]
  -- text $ show <<< map (map (Record.delete (Proxy :: _ "ref"))) <$> mbRoomEv
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
