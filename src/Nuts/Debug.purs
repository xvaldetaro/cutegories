module Nuts.Debug where


import Prelude

import Bolson.Core (envy)
import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Doku
import Deku.Listeners (click)
import Paraglider.Operator.MemoBeh (memoBeh)

nut :: ∀ l p. Domable l p
nut = Doku.do
  p /\ e' <- useState'
  e <- envy <<< memoBeh e' Nothing

  e # switcher D.div (empty) case _ of
    Nothing -> D.div_
      [ text_ "Nothing"
      , D.button  (click $ pure $ p $ Just 0) [text_ "To Just 0"]
      ]
    Just _ -> D.div_
      [ D.button  (click $ pure $ p Nothing) [text_ "To Nothing"]
      , D.button  (click (e <#> \v -> p ((_ + 1) <$> v))) [text_ "Increment"]
      , text $ show <$> e
      ]
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
