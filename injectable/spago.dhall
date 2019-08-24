{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "injectable"
, dependencies =
    [ "aff"
    , "argonaut"
    , "console"
    , "debug"
    , "effect"
    , "foreign"
    , "milkis"
    , "newtype"
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
