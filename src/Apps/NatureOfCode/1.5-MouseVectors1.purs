-- | Implements the vectors pointing at the mouse from https://www.youtube.com/watch?v=ttz05d8DSOs.
module Apps.NatureOfCode.MouseVectors1 (app) where

import Prelude
import Data.Maybe (Maybe(..))
import Graphics as G
import Model.Events (MouseEvent(..), MouseData)
import Model.Vector (Vector2, (<=>), (|/|))
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

type State
  = Vector2

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: State -> CanvasRuntime (Maybe State)
initialize _ = do
  G.setCanvasSize canvasSize
  G.background "black"
  pure Nothing

handleMouse :: MouseData -> State -> CanvasRuntime (Maybe State)
handleMouse mouse state = case mouse.event of
  MouseMove -> pure $ Just mouse.location
  _ -> pure Nothing

render :: State -> CanvasRuntime Unit
render state = do
  G.resetTransform
  G.translate $ canvasSize |/| 2.0
  let
    v = state - (200.0 <=> 200.0)
  G.setStrokeStyle "#FFFFFF64"
  G.setStrokeWidth 2.0
  G.line zero v

app :: CanvasApp State
app =
  (defaultApp zero)
    { initialize = initialize
    , render = render
    , handleMouse = handleMouse
    }
