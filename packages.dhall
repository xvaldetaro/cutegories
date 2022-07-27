let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220706/packages.dhall
        sha256:7a24ebdbacb2bfa27b2fc6ce3da96f048093d64e54369965a2a7b5d9892b6031

-- let overrides =
--       { hyrule-paraglider =
--         { dependencies =
--           [ "aff"
--           , "arrays"
--           , "console"
--           , "datetime"
--           , "effect"
--           , "either"
--           , "exceptions"
--           , "filterable"
--           , "foldable-traversable"
--           , "halogen-subscriptions"
--           , "hyrule"
--           , "integers"
--           , "js-timers"
--           , "maybe"
--           , "prelude"
--           , "refs"
--           , "tailrec"
--           , "tuples"
--           ]
--         , repo = "https://github.com/xvaldetaro/purescript-hyrule-paraglider"
--         , version = "v0.0.1"
--         }
--       }

in  upstream
  with hyrule-paraglider = ../hyrule-paraglider/spago.dhall as Location
