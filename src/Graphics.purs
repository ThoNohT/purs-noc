-- | Contains helper methods for drawing on the canvas.
module Graphics
  ( GraphicsContext
  , getCanvasSize
  , setCanvasSize
  , setFillStyle
  , setStrokeStyle
  , setStrokeWidth
  , getTransform
  , setTransform
  , resetTransform
  , translate
  , scale
  , background
  , fillRect
  , makeContextForElement
  , point
  , line
  ) where

import Prelude
import Data.Maybe (fromJust)
import Effect (Effect)
import Graphics.Canvas as GC
import Math (pi)
import Model.Vector (Vector2, getX, getY, (<=>))
import Partial.Unsafe (unsafePartial)

{- Context -}
-- | The graphics context that all operations can be done with.
data GraphicsContext
  = GraphicsContext GC.CanvasElement GC.Context2D

-- | Makes a graphics context for the element with the specified identifier.
makeContextForElement :: String -> Effect GraphicsContext
makeContextForElement elementId = do
  canvas_ <- unsafePartial fromJust <$> GC.getCanvasElementById elementId
  context_ <- GC.getContext2D canvas_
  pure $ GraphicsContext canvas_ context_

-- | Gets the canvas from a graphics context.
canvas :: GraphicsContext -> GC.CanvasElement
canvas (GraphicsContext c _) = c

-- | Gets the 2d context from a graphics context.
context :: GraphicsContext -> GC.Context2D
context (GraphicsContext _ c) = c

{- Canvas operations -}
-- | Gets the size of the current canvas.
getCanvasSize :: GraphicsContext -> Effect Vector2
getCanvasSize ctx = do
  size <- GC.getCanvasDimensions $ canvas ctx
  pure $ size.width <=> size.height

-- | Sets the size of the current canvas.
setCanvasSize :: GraphicsContext -> Vector2 -> Effect Unit
setCanvasSize ctx size = GC.setCanvasDimensions (canvas ctx) { width: getX size, height: getY size }

{- Styling -}
-- | Sets the current  fill style.
setFillStyle :: GraphicsContext -> String -> Effect Unit
setFillStyle ctx style = GC.setFillStyle (context ctx) style

-- | Sets the current stroke style.
setStrokeStyle :: GraphicsContext -> String -> Effect Unit
setStrokeStyle ctx style = GC.setStrokeStyle (context ctx) style

-- | Sets the current stroke width.
setStrokeWidth :: GraphicsContext -> Number -> Effect Unit
setStrokeWidth ctx width = GC.setLineWidth (context ctx) width

{- Transforms -}
-- Gets the current transform
foreign import getTransform_ :: GC.Context2D -> Effect GC.Transform

-- | Returns the current transform.
getTransform :: GraphicsContext -> Effect GC.Transform
getTransform ctx = getTransform_ (context ctx)

-- | Sets the current transform.
setTransform :: GraphicsContext -> GC.Transform -> Effect Unit
setTransform ctx transform = GC.setTransform (context ctx) transform

-- | Resets the current transform to the identity matrix.
resetTransform :: GraphicsContext -> Effect Unit
resetTransform ctx = setTransform ctx { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0 }

-- | Translates the entire canvas.
translate :: GraphicsContext -> Vector2 -> Effect Unit
translate ctx offset = GC.translate (context ctx) { translateX: getX offset, translateY: getY offset }

-- | Scales the entire canvas.
scale :: GraphicsContext -> Vector2 -> Effect Unit
scale ctx factor = GC.scale (context ctx) { scaleX: getX factor, scaleY: getY factor }

{- Drawing -}
-- | Draws the entire canvas with the specified style.
background :: GraphicsContext -> String -> Effect Unit
background ctx style = do
  oldTransform <- getTransform ctx
  _ <- resetTransform ctx
  dimensions <- getCanvasSize ctx
  _ <- GC.setFillStyle (context ctx) style -- TODO: Find a way to reset the fill style.
  _ <- fillRect ctx zero dimensions
  GC.setTransform (context ctx) oldTransform

-- | Fills the specified area with the specified style.
fillRect :: GraphicsContext -> Vector2 -> Vector2 -> Effect Unit
fillRect ctx base size = do
  GC.fillRect (context ctx) { x: getX base, y: getY base, width: getX size, height: getY size }

-- | Draws a point at the specified location, with the specified radius.
point :: GraphicsContext -> Vector2 -> Number -> Effect Unit
point ctx position radius = do
  _ <- GC.beginPath $ context ctx
  _ <- GC.arc (context ctx) { x: getX position, y: getY position, radius: radius, start: 0.0, end: 2.0 * pi }
  GC.fill $ context ctx
  GC.stroke $ context ctx

-- | Draws a line from the specified location to the specified location.
line :: GraphicsContext -> Vector2 -> Vector2 -> Effect Unit
line ctx from to = do
  _ <- GC.beginPath (context ctx)
  _ <- GC.moveTo (context ctx) (getX from) (getY from)
  _ <- GC.lineTo (context ctx) (getX to) (getY to)
  GC.stroke (context ctx)
