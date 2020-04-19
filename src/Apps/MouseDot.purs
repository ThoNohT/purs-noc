module Apps.MouseDot (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics as G
import Model.Vector (Vector2, (<=>), diagonal)

-- | The state just contains the location of the mouse.
type State
  = Vector2

-- | This value is used to set and get the canvas size everywhere else.
canvasSize :: Vector2
canvasSize = 1024.0 <=> 768.0

-- | Sets the canvas to the desired size.
initialize :: G.GraphicsContext -> State -> Effect (Maybe State)
initialize ctx state = G.setCanvasSize ctx canvasSize >>= const (pure Nothing)

-- | Renders a white background, and a red square around the mouse position.
render :: G.GraphicsContext -> State -> Effect Unit
render ctx state = do
  G.setFillStyle ctx "white"
  G.fillRect ctx zero canvasSize
  G.setFillStyle ctx "red"
  G.fillRect ctx (state - diagonal 8.0) (diagonal 16.0)

-- | Define the main application.
app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec (0.0 <=> 0.0))
        { initialize = initialize
        , render = render
        , updateInterval = 10000
        , handleMouse = \e _ -> pure $ Just e.location
        }
