{ name = "dashboard"
, dependencies =
  [ "argonaut"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "formatters"
  , "generics-rep"
  , "js-date"
  , "js-timers"
  , "milkis"
  , "now"
  , "numbers"
  , "profunctor-lenses"
  , "psci-support"
  , "react-basic-hooks"
  , "routing"
  , "routing-duplex"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
