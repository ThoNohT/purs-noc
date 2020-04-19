-- | A module implementation of the Walker class in https://www.youtube.com/watch?v=bKEaK7WNLzM.
module Apps.NatureOfCode.Randomwalker3.Walker where

import Prelude
import Effect (Effect)
import Graphics as G
import Model.Vector (Vector2, randomVector, (|/|))

-- | The walker type, contains just an x, and a y coordinate.
type Walker
  = Vector2

-- | Initializes a walker.
init :: Vector2 -> Walker
init canvasSize = canvasSize |/| 2.0

-- | Updates the walker, performing a walk step.
update :: Walker -> Effect Walker
update walker = do
  diff <- randomVector
  pure $ walker + diff

-- | Renders the walker to the provided context.
render :: G.GraphicsContext -> Walker -> Effect Unit
render ctx walker = do
  _ <- G.setFillStyle ctx "#FFFFFF64"
  G.point ctx walker 2.0
