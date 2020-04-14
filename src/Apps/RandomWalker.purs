module Apps.RandomWalker (app) where

import Prelude
import Effect.Console (logShow)
import App as App

app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec { x: 0, y: 0 })
        { render = (\state -> logShow state.x) }
