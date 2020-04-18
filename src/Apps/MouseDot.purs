module Apps.MouseDot (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas as GC
import Model (Point)

-- | The state just contains the location of the mouse.
type State
  = Point

-- | This value is used to set and get the canvas size everywhere  else.
canvasSize :: GC.Dimensions
canvasSize = { width: 1024.0, height: 768.0 }

-- | Sets the canvas to the desired size.
initialize :: GC.CanvasElement -> State -> Effect Unit
initialize canvas state = GC.setCanvasDimensions canvas canvasSize

-- | Renders a white background, and a red square around the mouse position.
render :: GC.Context2D -> State -> Effect Unit
render ctx state = do
  _ <- GC.setFillStyle ctx "white"
  _ <- GC.fillRect ctx { width: canvasSize.width, height: canvasSize.height, x: 0.0, y: 0.0 }
  _ <- GC.setFillStyle ctx "red"
  GC.fillRect ctx { width: 16.0, height: 16.0, x: state.x - 8.0, y: state.y - 8.0 }

-- | Define the main application.
app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec { x: 0.0, y: 0.0 })
        { initialize = initialize
        , render = render
        , updateInterval = 10000
        , handleMouse = \e _ -> pure $ Just e.location
        }
