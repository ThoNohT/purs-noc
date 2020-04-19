-- | Implements the random vectors from https://www.youtube.com/watch?v=jupjuq9Jl-M.
module Apps.NatureOfCode.RandomVectors (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics (background, line, resetTransform)
import Graphics.Canvas as GC
import Model.Vector (Vector2, randomVector, (|*|))

-- | The state just contains the location of the mouse.
type State
  = Vector2

-- | This value is used to set and get the canvas size everywhere  else.
canvasSize :: GC.Dimensions
canvasSize = { width: 400.0, height: 400.0 }

-- | Sets the canvas to the desired size.
initialize :: GC.CanvasElement -> State -> Effect Unit
initialize canvas _ = do
  _ <- GC.setCanvasDimensions canvas canvasSize
  background canvas "black"

tick :: State -> Effect (Maybe State)
tick state = do
  v <- randomVector
  pure $ Just $ v |*| 100.0

-- | Renders a white background, and a red square around the mouse position.
render :: GC.Context2D -> State -> Effect Unit
render ctx state = do
  _ <- resetTransform ctx
  --   _ <- fillArea ctx zero (canvasSize.width <=> canvasSize.height) "black"
  _ <- GC.translate ctx { translateX: canvasSize.width / 2.0, translateY: canvasSize.height / 2.0 }
  _ <- GC.setStrokeStyle ctx "white"
  _ <- GC.setLineWidth ctx 4.0
  line ctx zero state

-- | Define the main application.
app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec zero)
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        }
