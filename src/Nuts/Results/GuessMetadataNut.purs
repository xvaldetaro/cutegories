module Nuts.Results.GuessMetadataNut where

import Prelude

import Control.Alt ((<|>))
import Core.Room.ValidationManager (toggleInvalid, togglePairRepeated)
import Data.Array (head, length, (:))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple.Nested ((/\))
import Deku.Control (switcher_, text_)
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState, useState')
import Deku.Do as Doku
import Deku.Listeners (click)
import Effect.Aff (launchAff_)
import FRP.Event (Event)
import Models.Models (GuessMetadata)
import Nuts.Dumb.Btn (grayCss, redCss, tealCss)
import Nuts.Dumb.Btn as Btn
import Nuts.Results.ValidationTable (ValidationTable, getRepetitions, hasAnyRepetition, isInvalid, isRepeated)
import Nuts.Room.ResultsEnv (ResultsEnv)
import Platform.Deku.Html (bangCss, combineCss, css, hideIf, showIf)
import Platform.Deku.Misc (cleanFbAff, ife)

nut :: ResultsEnv -> Event ValidationTable -> GuessMetadata -> Nut
nut {env: env@{fb}, roomId} vtEv {text, players, similars} = Doku.do
  pushCollapse /\ collapseEv <- useState true

  let
    playerPill {name} =
      D.li ( bangCss "ml-1 mr-1 mb-1 px-2 flex font-medium text-sm rounded-full bg-gray-600 " )
        [ text_ name ]

    doToggleInvalid = launchAff_ <<< cleanFbAff env <<< toggleInvalid fb roomId text
    doTogglePairRepeated t1 t2 = launchAff_ <<< void <<< togglePairRepeated fb roomId t1 t2

    mostSimilarRating :: Number
    mostSimilarRating = fromMaybe 0.0 ((_.rating) <$> head similars)

    similarityIconCss similarity
      | similarity > 0.7 = css "text-red-400"
      | similarity > 0.1 = css "text-yellow-400"
      | otherwise = css "hidden"

    alertIcon hideEv similarity = D.i (combineCss [pure $ css "ion-alert-circled mr-2", extraCss]) []
      where
      extraCss = hideEv <#> if _ then css "hidden" else similarityIconCss similarity

    renderSimilars =
      D.div
        (combineCss
          [ pure $ css "ml-6 mr-3 pt-2 mb-2 flex flex-col items-start border-t-2 border-gray-800"
          , hideIf <$> collapseEv
          ]
        )
        [ D.button
            ( (click $ doToggleInvalid <$> vtEv)
              <|> bangCss "flex items-center mr-2 rounded-md bg-gray-600 px-2 mb-2"
            )
            [ D.i
                (combineCss
                  [pure $ css "mr-1"
                  , isInvalidEv <#> ife
                      (css "ion-checkmark text-teal-400") (css "ion-close-circled text-red-400")
                  ]
                ) []

            , DC.text $ ife "Mark Valid" "Mark Invalid" <$> isInvalidEv
            ]
        , D.div (bangCss "flex flex-wrap gap-y-2 ")
            ( (D.span (bangCss "text-sm mr-2") [text_ "Mark copies: "])
              : (renderSimilar <$> similars)
            )
        ]

    isRepeatedEv :: String -> String -> Event Boolean
    isRepeatedEv g1 g2 = vtEv <#> isRepeated g1 g2

    isInvalidEv = vtEv <#> isInvalid text

    selfHasAnyRepetitionEv = vtEv <#> hasAnyRepetition text

    collapseButton =
      D.i ( combineCss [pure $ css "text-lg mr-3", ionIconCssEv]) []
      where
      ionIconCssEv = collapseEv <#> ife "ion-chevron-right" "ion-chevron-down"

    renderSimilar {target, rating} =
       D.button
          ( (combineCss
              [ pure $ css "mr-2 px-2 text-sm flex items-center" <> btnBaseCss
                <> css ""
              , isRepeatedEv text target <#> ife yellowButtonColor Btn.grayCss
              , vtEv <#> isInvalid target >>> (ife (css "hidden") "")
              ]
            ) <|> (click $ doTogglePairRepeated text target <$> vtEv)
          )
          [ alertIcon (pure false) rating, text_ target ]

  D.div
    ( combineCss
      [ vtEv <#> bgColorCss
      , pure $ css "mx-3 rounded-md flex flex-col mb-1"
      ]
    )
    [ D.div
        ( bangCss "ml-3 mt-1 mb-1 flex items-center flex-wrap"
          <|> (click $ pushCollapse <<< not <$> collapseEv)
        )
        ([ collapseButton
        , alertIcon selfHasAnyRepetitionEv mostSimilarRating
        , D.span (bangCss " mr-2 text-white font-semibold") [text_ text]
        ] <> (playerPill <$> players))
    , D.div (bangCss "rounded-lg flex flex-wrap")
      [renderSimilars]
    ]

  where
  bgColorCss :: ValidationTable -> String
  bgColorCss vt
    | isInvalid text vt = css "bg-red-900"
    | selfHasAnyRepetition vt = css "bg-yellow-900"
    | otherwise = css "bg-gray-700 "

  borColorCss :: ValidationTable -> String
  borColorCss vt
    | isInvalid text vt = css "border-red-900 border-2"
    | selfHasAnyRepetition vt = css "border-yellow-900 border-2"
    | otherwise = css ""

  selfHasAnyRepetition vt = length players > 1 || hasAnyRepetition text vt

yellowButtonColor :: String
yellowButtonColor = css "bg-yellow-700 hover:bg-yellow-600"

btnBaseCss :: String
btnBaseCss = css "text-white cursor-pointer rounded-md shadow-md font-semibold"

-- similarityColor :: Number -> String
-- similarityColor rating
--   | rating < 0.4 -> css ""

