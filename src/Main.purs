module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
{-
    Import a specific app here as ActiveApp to run this app.
 -}
import Apps.MouseDot as ActiveApp

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ActiveApp.app unit body
