-- | Implements the vector pointing at the mouse with normalized length from https://www.youtube.com/watch?v=ttz05d8DSOs.
module Apps.NatureOfCode.MouseVectors2 (app) where

import Prelude

import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics (fillArea, line, resetTransform)
import Graphics.Canvas as GC
import Model.Events (MouseEvent(..), MouseData)
import Model.Vector (Vector2, setMagnitude, (<=>))

-- | The state just contains the location of the mouse.
type State
  = Vector2

-- | This value is used to set and get the canvas size everywhere  else.
canvasSize :: GC.Dimensions
canvasSize = { width: 400.0, height: 400.0 }

-- | Sets the canvas to the desired size.
initialize :: GC.CanvasElement -> State -> Effect (Maybe State)
initialize canvas _ = do
  _ <- GC.setCanvasDimensions canvas canvasSize
  pure Nothing

tick :: State -> Effect (Maybe State)
tick state = do
  pure Nothing

handleMouse :: MouseData -> State -> Effect (Maybe State)
handleMouse mouse state = case mouse.event of
  MouseMove -> pure $ Just mouse.location
  _ -> pure Nothing

-- | Renders a white background, and a red square around the mouse position.
render :: GC.Context2D -> State -> Effect Unit
render ctx state = do
  _ <- resetTransform ctx
  _ <- fillArea ctx zero (canvasSize.width <=> canvasSize.height) "black"
  _ <- GC.translate ctx { translateX: canvasSize.width / 2.0, translateY: canvasSize.height / 2.0 }

  let v = state - (200.0 <=> 200.0) # setMagnitude 50.0

  _ <- GC.setStrokeStyle ctx "white"
  _ <- GC.setLineWidth ctx 2.0
  line ctx zero v

-- | Define the main application.
app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec zero)
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        , handleMouse = handleMouse
        }
