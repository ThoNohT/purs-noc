module RandomWalker (app) where

import Prelude

import App as App
import Effect.Console (log)

app :: App.CanvasAppType
app = App.app
    { initialState: { x: 0, y: 0 }
    , tick: (\_ state -> state)
    , handleKeyboard: (\_ state -> state)
    , handleMouse: (\_ state -> state)
    , render: (\state -> (log (show state.x)))
    }