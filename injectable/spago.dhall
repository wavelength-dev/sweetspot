{ sources =
    [ "src/**/*.purs" ]
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
    , "web-dom"
    , "web-html"
    , "web-storage"
    , "web-xhr"
    ]
, packages =
    ./packages.dhall
}
