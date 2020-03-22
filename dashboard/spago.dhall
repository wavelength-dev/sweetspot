{ name = "dashboard"
, dependencies =
    [ "argonaut"
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
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
