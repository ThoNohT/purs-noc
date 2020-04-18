-- | A module implementation of the Walker class in https://www.youtube.com/watch?v=bKEaK7WNLzM.
module Apps.Randomwalker3.Walker (Walker, update, render) where

import Prelude
import Model (Point)
import Effect (Effect)
import Graphics.Canvas as GC
import Effect.Random as Random
import Graphics (point)

-- | The walker type, contains just an x, and a y coordinate.
type Walker
  = Point

-- | Updates the walker, performing a walk step.
update :: Walker -> Effect Walker
update walker = do
  diffX <- Random.randomRange (-1.0) 1.0
  diffY <- Random.randomRange (-1.0) 1.0
  pure $ walker { x = walker.x + diffX, y = walker.y + diffY }

-- | Renders the walker to the provided context.
render :: GC.Context2D -> Walker -> Effect Unit
render ctx walker = do
  _ <- GC.setFillStyle ctx "#FFFFFF64"
  point ctx walker 2.0
