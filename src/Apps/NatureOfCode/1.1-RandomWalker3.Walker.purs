-- | A module implementation of the Walker class in https://www.youtube.com/watch?v=bKEaK7WNLzM.
module Apps.NatureOfCode.Randomwalker3.Walker where

import Prelude
import Effect (Effect)
import Graphics (point)
import Graphics.Canvas as GC
import Model.Vector (Vector2, randomVector, (<=>))

-- | The walker type, contains just an x, and a y coordinate.
type Walker
  = Vector2

-- | Initializes a walker.
init :: GC.Dimensions -> Walker
init canvasSize = ((canvasSize.width / 2.0) <=> (canvasSize.height / 2.0))

-- | Updates the walker, performing a walk step.
update :: Walker -> Effect Walker
update walker = do
  diff <- randomVector
  pure $ walker + diff

-- | Renders the walker to the provided context.
render :: GC.Context2D -> Walker -> Effect Unit
render ctx walker = do
  _ <- GC.setFillStyle ctx "#FFFFFF64"
  point ctx walker 2.0
