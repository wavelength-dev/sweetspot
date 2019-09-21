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
    , "ordered-collections"
    , "prelude"
    , "psci-support"
    , "record"
    , "test-unit"
    , "web-dom"
    , "web-html"
    , "web-storage"
    , "web-xhr"
    ]
, packages =
    ./packages.dhall
}
