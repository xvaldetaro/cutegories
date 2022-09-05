let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220822/packages.dhall
        sha256:908b4ffbfba37a0a4edf806513a555d0dbcdd0cde7abd621f8d018d2e8ecf828

let overrides =
      { hyrule-paraglider = ../hyrule-paraglider/spago.dhall as Location
      , deku = ../deku/spago.dhall as Location
      ,bolson =
        { dependencies = [ "arrays" ]
        , repo = "https://github.com/mikesol/purescript-bolson.git"
        , version = "main"
        }
      -- , deku =
      --   { dependencies =
      --     [ "arrays"
      --     , "bolson"
      --     , "control"
      --     , "effect"
      --     , "fast-vect"
      --     , "filterable"
      --     , "foldable-traversable"
      --     , "foreign-object"
      --     , "heterogeneous"
      --     , "hyrule"
      --     , "maybe"
      --     , "monoid-extras"
      --     , "newtype"
      --     , "ordered-collections"
      --     , "prelude"
      --     , "profunctor"
      --     , "quickcheck"
      --     , "record"
      --     , "refs"
      --     , "safe-coerce"
      --     , "st"
      --     , "strings"
      --     , "transformers"
      --     , "unsafe-coerce"
      --     , "web-dom"
      --     , "web-events"
      --     , "web-html"
      --     ]
      --   , repo = "https://github.com/mikesol/purescript-deku.git"
      --   , version = "main"
      --   }
      , hyrule =
        { dependencies = [ "arrays", "aff"
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
          , "web-html" ]
        , repo = "https://github.com/mikesol/purescript-hyrule.git"
        , version = "master"
        }
      }

in  upstream // overrides
