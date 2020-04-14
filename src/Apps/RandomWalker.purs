module Apps.RandomWalker (app) where

import Prelude
import Effect.Console (log)
import App as App
import Debug as Debug


app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec "")
        { render = \state -> log state
        , handleKeyboard = \e s -> Debug.unsafeLog e.keyCode
        }
