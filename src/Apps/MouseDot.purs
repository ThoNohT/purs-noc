module Apps.MouseDot (app) where

import Prelude

import Data.Maybe (Maybe(..))
import Graphics (GraphicsContext)
import Graphics as G
import Model.Vector (Vector2, (<=>), diagonal)
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

-- | The state just contains the location of the mouse.
type State
  = Vector2

-- | This value is used to set and get the canvas size everywhere else.
canvasSize :: Vector2
canvasSize = 1024.0 <=> 768.0

-- | Sets the canvas to the desired size.
initialize :: State -> CanvasRuntime GraphicsContext (Maybe State)
initialize state = do
  G.setCanvasSize canvasSize >>= const (pure Nothing)

-- | Renders a white background, and a red square around the mouse position.
render :: State -> CanvasRuntime GraphicsContext Unit
render state = do
  G.setFillStyle "white"
  G.fillRect zero canvasSize
  G.setFillStyle "red"
  G.fillRect (state - diagonal 8.0) (diagonal 16.0)

-- | Define the main application.
app :: CanvasApp GraphicsContext State
app =
  (defaultApp (0.0 <=> 0.0))
    { initialize = initialize
    , render = render
    , updateInterval = 10000
    , handleMouse = \e _ -> pure $ Just e.location
    }
