-- | Contains helper methods for drawing on the canvas.
module Graphics (point, background) where

import Prelude
import Effect (Effect)
import Graphics.Canvas as GC
import Model (Point)
import Math (pi)

-- | Draws the entire canvas with the specified style.
background :: GC.CanvasElement -> String -> Effect Unit
background canvas style = do
  dimensions <- GC.getCanvasDimensions canvas
  ctx <- GC.getContext2D canvas
  _ <- GC.setFillStyle ctx style
  GC.fillRect ctx { width: dimensions.width, height: dimensions.height, x: 0.0, y: 0.0 }

-- | Fills the specified area with the specified style.
fillArea :: GC.Context2D -> Point -> Point -> String -> Effect Unit
fillArea ctx base size style = do
  _ <- GC.setFillStyle ctx style
  GC.fillRect ctx { width: size.x, height: size.y, x: 0.0, y: 0.0 }

-- | Draws a point at the specified location, with the specified radius.
point :: GC.Context2D -> Point -> Number -> Effect Unit
point ctx position radius = do
  _ <- GC.beginPath ctx
  _ <- GC.arc ctx { x: position.x, y: position.y, radius: radius, start: 0.0, end: 2.0 * pi }
  GC.fill ctx
