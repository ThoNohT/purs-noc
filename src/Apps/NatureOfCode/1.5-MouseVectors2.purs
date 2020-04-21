-- | Implements the vector pointing at the mouse with normalized length from https://www.youtube.com/watch?v=ttz05d8DSOs.
module Apps.NatureOfCode.MouseVectors2 (app) where

import Prelude
import Data.Maybe (Maybe(..))
import Graphics as G
import Model.Events (MouseEvent(..), MouseData)
import Model.Vector (Vector2, setMagnitude, (<=>), (|/|))
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

type State
  = Vector2

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: State -> CanvasRuntime (Maybe State)
initialize _ = do
  G.setCanvasSize canvasSize
  G.translate $ canvasSize |/| 2.0
  pure Nothing

handleMouse :: MouseData -> State -> CanvasRuntime (Maybe State)
handleMouse mouse state = case mouse.event of
  MouseMove -> pure $ Just mouse.location
  _ -> pure Nothing

render :: State -> CanvasRuntime Unit
render state = do
  G.background "black"
  let
    v = state - (200.0 <=> 200.0) # setMagnitude 50.0
  G.setStrokeStyle "white"
  G.setStrokeWidth 2.0
  G.line zero v

app :: CanvasApp State
app =
  (defaultApp zero)
    { initialize = initialize
    , render = render
    , handleMouse = handleMouse
    }
