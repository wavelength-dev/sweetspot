{ name = "dashboard"
, dependencies =
<<<<<<< HEAD
    [ "argonaut"
    , "console"
    , "effect"
    , "generics-rep"
    , "milkis"
    , "profunctor-lenses"
    , "psci-support"
    , "react-basic-hooks"
    , "routing"
    , "routing-duplex"
    , "web-dom"
    , "web-html"
    ]
=======
  [ "argonaut"
  , "browser-cookies"
  , "console"
  , "debug"
  , "effect"
  , "generics-rep"
  , "milkis"
  , "profunctor-lenses"
  , "psci-support"
  , "react-basic-hooks"
  , "routing"
  , "routing-duplex"
  , "web-dom"
  , "web-html"
  ]
>>>>>>> add experiment list page
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
