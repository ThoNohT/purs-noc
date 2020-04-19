-- | Contains helper methods for drawing on the canvas.
module Graphics where

import Prelude
import Effect (Effect)
import Graphics.Canvas as GC
import Math (pi)
import Model.Vector (Vector2, getX, getY)

-- | Draws the entire canvas with the specified style.
background :: GC.CanvasElement -> String -> Effect Unit
background canvas style = do
  dimensions <- GC.getCanvasDimensions canvas
  ctx <- GC.getContext2D canvas
  _ <- GC.setFillStyle ctx style
  GC.fillRect ctx { width: dimensions.width, height: dimensions.height, x: 0.0, y: 0.0 }

-- | Fills the specified area with the specified style.
fillArea :: GC.Context2D -> Vector2 -> Vector2 -> String -> Effect Unit
fillArea ctx base size style = do
  _ <- GC.setFillStyle ctx style
  GC.fillRect ctx { width: getX size, height: getY size, x: 0.0, y: 0.0 }

-- | Draws a point at the specified location, with the specified radius.
point :: GC.Context2D -> Vector2 -> Number -> Effect Unit
point ctx position radius = do
  _ <- GC.beginPath ctx
  _ <- GC.arc ctx { x: getX position, y: getY position, radius: radius, start: 0.0, end: 2.0 * pi }
  GC.fill ctx
  GC.stroke ctx

-- | Draws a line from the specified location to the specified location.
line :: GC.Context2D -> Vector2 -> Vector2 -> Effect Unit
line ctx from to = do
  _ <- GC.beginPath ctx
  _ <- GC.moveTo ctx (getX from) (getY from)
  _ <- GC.lineTo ctx (getX to) (getY to)
  GC.stroke ctx

-- | Resets the current transform to the identity matrix.
resetTransform :: GC.Context2D -> Effect Unit
resetTransform ctx = GC.setTransform ctx { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0 }
