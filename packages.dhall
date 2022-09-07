let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220901/packages.dhall
        sha256:f1531b29c21ac437ffe5666c1b6cc76f0a9c29d3c9d107ff047aa2567744994f

let overrides =
  { hyrule-paraglider = ../hyrule-paraglider/spago.dhall as Location
  , bolson =
    { dependencies = [ "arrays" ]
    , repo = "https://github.com/mikesol/purescript-bolson.git"
    , version = "v0.2.0"
    }
  , deku =
    { dependencies =
      [ "aff"
      , "arrays"
      , "bolson"
      , "control"
      , "effect"
      , "hyrule"
      , "fast-vect"
      , "filterable"
      , "foldable-traversable"
      , "foreign-object"
      , "heterogeneous"
      , "maybe"
      , "monoid-extras"
      , "newtype"
      , "ordered-collections"
      , "prelude"
      , "profunctor"
      , "quickcheck"
      , "record"
      , "refs"
      , "safe-coerce"
      , "st"
      , "strings"
      , "transformers"
      , "unsafe-coerce"
      , "web-dom"
      , "web-events"
      , "web-html"
      ]
    , repo = "https://github.com/mikesol/purescript-deku.git"
    , version = "5cadd4f3a87181025a544ae74b5ca50d5c9abd3a"
    }
  , hyrule =
    { dependencies =
      [ "arrays"
      , "aff"
      , "aff-promise"
      , "arraybuffer-types"
      , "avar"
      , "bolson"
      , "control"
      , "convertable-options"
      , "effect"
      , "either"
      , "monoid-extras"
      , "exceptions"
      , "fast-vect"
      , "foldable-traversable"
      , "foreign"
      , "foreign-object"
      , "homogeneous"
      , "indexed-monad"
      , "integers"
      , "js-timers"
      , "lists"
      , "maybe"
      , "newtype"
      , "numbers"
      , "ordered-collections"
      , "prelude"
      , "profunctor"
      , "profunctor-lenses"
      , "random"
      , "refs"
      , "safe-coerce"
      , "simple-json"
      , "sized-vectors"
      , "tuples"
      , "type-equality"
      , "typelevel"
      , "typelevel-prelude"
      , "unsafe-coerce"
      , "unsafe-reference"
      , "variant"
      , "web-events"
      , "web-uievents"
      , "web-file"
      , "web-html"
      ]
    , repo = "https://github.com/mikesol/purescript-hyrule.git"
    , version = "v2.2.0"
    }
  }

in  upstream // overrides
