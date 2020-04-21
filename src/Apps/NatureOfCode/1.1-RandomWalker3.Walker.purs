-- | A module implementation of the Walker class in https://www.youtube.com/watch?v=bKEaK7WNLzM.
module Apps.NatureOfCode.Randomwalker3.Walker where

import Prelude
import Graphics as G
import Model.Vector (Vector2, randomVector, (|/|))
import Toolkit (CanvasRuntime)

-- | The walker type, contains just an x, and a y coordinate.
type Walker
  = Vector2

-- | Initializes a walker.
init :: Vector2 -> Walker
init canvasSize = canvasSize |/| 2.0

-- | Updates the walker, performing a walk step.
update :: Walker -> CanvasRuntime Walker
update walker = do
  diff <- randomVector
  pure $ walker + diff

-- | Renders the walker to the provided context.
render :: Walker -> CanvasRuntime Unit
render walker = do
  G.setFillStyle "#FFFFFF64"
  G.circle walker 4.0
