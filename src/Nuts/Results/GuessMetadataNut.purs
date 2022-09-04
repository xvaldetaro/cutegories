module Nuts.Results.GuessMetadataNut where

import Prelude

import Control.Alt ((<|>))
import Core.Room.ValidationManager (toggleInvalid, togglePairRepeated)
import Data.Array (length, (:))
import Deku.Control (text_)
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Listeners (click)
import Effect.Aff (launchAff_)
import FRP.Event (Event)
import Models.Models (GuessMetadata)
import Nuts.Dumb.Btn (grayCss, redCss, tealCss)
import Nuts.Dumb.Btn as Btn
import Nuts.Results.ValidationTable (ValidationTable, hasAnyRepetition, isInvalid, isRepeated)
import Nuts.Room.ResultsEnv (ResultsEnv)
import Platform.Deku.Html (bangCss, combineCss, css, hiddenIf)
import Platform.Deku.Misc (cleanFbAff, ife)

nut :: ResultsEnv -> Event ValidationTable -> GuessMetadata -> Nut
nut {env: env@{fb}, roomId} vtEv {text, players, similars} = D.div
  (combineCss
    [ pure $ css "flex flex-col items-start border rounded-t-lg rounded-b-md mb-2"
    , borColorCss <$> vtEv
    ]
  )
  [ renderCardHeader
  , D.div (combineCss [pure $ css "flex", hiddenIf <$> isInvalidEv text]) [renderSimilars]
  ]

  where
  -- doToggleInvalid = launchAff_ <<< cleanFbAff env <<< toggleInvalid fb roomId text
  -- doTogglePairRepeated t1 t2 = launchAff_ <<< cleanFbAff env <<< togglePairRepeated fb roomId t1 t2
  doToggleInvalid = launchAff_ <<< void <<< toggleInvalid fb roomId text
  doTogglePairRepeated t1 t2 = launchAff_ <<< void <<< togglePairRepeated fb roomId t1 t2

  renderCardHeader = D.div
    (combineCss
      [pure $ css "rounded-md rounded-b-none w-full flex items-center"
      , bgColorCss <$> vtEv
      ]
    )
    [ D.span (bangCss "text-white font-semibold mx-3") [text_ text]
    , D.div (bangCss "flex-grow flex-wrap flex") (renderPlayerName <$> players)
    , D.button
      ( (combineCss
          [ pure $ css "px-2 py-0" <> btnBaseCss
          , vtEv <#> isInvalid text >>> ife grayCss redCss
          ]
        )
          <|> (click $ doToggleInvalid <$> vtEv)
      )
      [DC.text $ (ife "Set Valid" "Set Invalid") <$> isInvalidEv text]
    ]

  renderPlayerName {name} = D.span (bangCss "mr-2 text-xs text-gray-100") [text_ name]

  renderSimilars = D.div (bangCss "gap-y-2 mx-3 my-2 flex items-end flex-wrap")
    ((D.span (bangCss "text-sm mr-2") [text_ "Mark copies: "]) : (renderSimilar <$> similars))

  isInvalidEv :: String -> Event Boolean
  isInvalidEv g1 = vtEv <#> isInvalid g1

  isRepeatedEv :: String -> String -> Event Boolean
  isRepeatedEv g1 g2 = vtEv <#> isRepeated g1 g2

  renderSimilar {target, rating} = D.button
    ( (combineCss
        [ pure $ css "mr-3 px-2 text-sm" <> btnBaseCss
          <> css ""
        , isRepeatedEv text target <#> ife yellowButtonColor Btn.grayCss
        , vtEv <#> isInvalid target >>> (ife (css "hidden") "")
        ]
      ) <|> (click $ doTogglePairRepeated text target <$> vtEv)
    )
    [ text_ target ]

  bgColorCss :: ValidationTable -> String
  bgColorCss vt
    | isInvalid text vt = css "bg-red-400"
    | selfHasAnyRepetition vt = css "bg-yellow-700"
    | otherwise = css "bg-teal-700"

  borColorCss :: ValidationTable -> String
  borColorCss vt
    | isInvalid text vt = css "border-red-400"
    | selfHasAnyRepetition vt = css "border-yellow-700"
    | otherwise = css "border-teal-700"

  selfHasAnyRepetition vt = length players > 1 || hasAnyRepetition text vt

yellowButtonColor :: String
yellowButtonColor = css "bg-yellow-700 hover:bg-yellow-600"

btnBaseCss :: String
btnBaseCss = css "text-white cursor-pointer rounded-md shadow-md font-semibold"
