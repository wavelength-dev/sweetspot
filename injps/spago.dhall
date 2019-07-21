{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "injps"
, dependencies =
    [ "aff"
    , "argonaut"
    , "console"
    , "debug"
    , "effect"
    , "foreign"
    , "generics-rep"
    , "milkis"
    , "numbers"
    , "prelude"
    , "psci-support"
    , "spec"
    , "web-dom"
    , "web-html"
    , "web-storage"
    , "web-xhr"
    ]
, packages =
    ./packages.dhall
}
