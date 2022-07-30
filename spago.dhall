{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arrays"
  , "bifunctors"
  , "bolson"
  , "console"
  , "const"
  , "control"
  , "datetime"
  , "deku"
  , "effect"
  , "either"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-hooks"
  , "halogen-store"
  , "halogen-subscriptions"
  , "hyrule"
  , "hyrule-paraglider"
  , "lists"
  , "maybe"
  , "monoid-extras"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "refs"
  , "routing"
  , "routing-duplex"
  , "safe-coerce"
  , "simple-json"
  , "string-parsers"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
