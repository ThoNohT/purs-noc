module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Framework.App (create)
{-
    Import a specific app here as ActiveApp to run this app.
 -}
import Apps.NatureOfCode.GravitationalAttraction as ActiveApp

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (create ActiveApp.app) unit body
