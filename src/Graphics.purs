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
  , circle
  , ellipse
  , line
  ) where

import Prelude
import Data.Maybe (fromJust)
import Effect (Effect)
import Graphics.Canvas as GC
import Math (pi)
import Model.Vector (Vector2, diagonal, getX, getY, (<=>))
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
-- | Sets the current fill style.
setFillStyle :: GraphicsContext -> String -> Effect Unit
setFillStyle ctx style = GC.setFillStyle (context ctx) style

-- | Gets the current fill style.
getFillStyle :: GraphicsContext -> Effect String
getFillStyle ctx = getFillStyle_ (context ctx)

-- | Sets the current stroke style.
setStrokeStyle :: GraphicsContext -> String -> Effect Unit
setStrokeStyle ctx style = GC.setStrokeStyle (context ctx) style

-- | Gets the current stroke style.
getStrokeStyle :: GraphicsContext -> Effect String
getStrokeStyle ctx = getStrokeStyle_ (context ctx)

-- | Sets the current stroke width.
setStrokeWidth :: GraphicsContext -> Number -> Effect Unit
setStrokeWidth ctx width = GC.setLineWidth (context ctx) width

-- | Gets the current stroke width.
getStrokeWidth :: GraphicsContext -> Effect Number
getStrokeWidth ctx = getStrokeWidth_ (context ctx)

{- Transforms -}
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
  -- Back-up old style/transform.
  oldTransform <- getTransform ctx
  oldFillStyle <- getFillStyle ctx
  -- Setup canvas.
  resetTransform ctx
  dimensions <- getCanvasSize ctx
  GC.setFillStyle (context ctx) style
  -- Fill.
  fillRect ctx zero dimensions
  -- Revert style/transform.
  setFillStyle ctx oldFillStyle
  setTransform ctx oldTransform

-- | Fills the specified area with the specified style.
fillRect :: GraphicsContext -> Vector2 -> Vector2 -> Effect Unit
fillRect ctx base size = do
  GC.fillRect (context ctx) { x: getX base, y: getY base, width: getX size, height: getY size }

-- | Draws a circle at the specified location, with the specified size.
ellipse :: GraphicsContext -> Vector2 -> Vector2 -> Effect Unit
ellipse ctx position size = do
  GC.beginPath $ context ctx
  ellipse_ (context ctx) (getX position) (getY position) (getX size) (getY size) 0.0 0.0 (2.0 * pi) false
  GC.fill $ context ctx
  GC.stroke $ context ctx

-- | Draws a circle at the specified location, with the specified radius.
circle :: GraphicsContext -> Vector2 -> Number -> Effect Unit
circle ctx position radius = ellipse ctx position (diagonal radius)

-- | Draws a line from the specified location to the specified location.
line :: GraphicsContext -> Vector2 -> Vector2 -> Effect Unit
line ctx from to = do
  GC.beginPath (context ctx)
  GC.moveTo (context ctx) (getX from) (getY from)
  GC.lineTo (context ctx) (getX to) (getY to)
  GC.stroke (context ctx)

{- Foreign imports -}
-- | Gets the current transform.
foreign import getTransform_ :: GC.Context2D -> Effect GC.Transform

-- | Gets the current fill style.
foreign import getFillStyle_ :: GC.Context2D -> Effect String

-- | Gets the current stroke style.
foreign import getStrokeStyle_ :: GC.Context2D -> Effect String

-- | Gets the current stroke width.
foreign import getStrokeWidth_ :: GC.Context2D -> Effect Number

-- | Traces an ellipse path. ctx -> x -> y -> radiusX -> radiusY -> rotation -> startAngle -> endAngle -> antiClockwise
foreign import ellipse_ :: GC.Context2D -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Boolean -> Effect Unit
