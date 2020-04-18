{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purs-noc"
, dependencies =
  [ "canvas"
  , "console"
  , "effect"
  , "halogen"
  , "integers"
  , "js-timers"
  , "math"
  , "partial"
  , "psci-support"
  , "random"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
