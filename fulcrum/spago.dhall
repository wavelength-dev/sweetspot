{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "fulcrum"
, dependencies =
    [ "aff"
    , "argonaut"
    , "console"
    , "effect"
    , "either"
    , "identity"
    , "milkis"
    , "now"
    , "ordered-collections"
    , "psci-support"
    , "test-unit"
    , "web-dom"
    , "web-html"
    , "web-storage"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
