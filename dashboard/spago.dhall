{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "argonaut"
    , "argonaut-generic"
    , "console"
    , "effect"
    , "generics-rep"
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
