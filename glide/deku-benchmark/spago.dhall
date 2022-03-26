{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "arrays"
  , "behaviors"
  , "datetime"
  , "debug"
  , "deku"
  , "deku-toplevel"
  , "effect"
  , "either"
  , "event"
  , "foldable-traversable"
  , "integers"
  , "math"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "refs"
  , "tuples"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
}
