module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import RandomWalker as RandomWalker

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI RandomWalker.app unit body