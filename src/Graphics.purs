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
import Control.Monad.State as S
import Data.Maybe (fromJust)
import Effect (Effect)
import Graphics.Canvas as GC
import Math (pi)
import Model.Vector (Vector2, diagonal, getX, getY, (<=>))
import Partial.Unsafe (unsafePartial)
import Toolkit (CanvasRuntime)

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
getCanvasSize :: CanvasRuntime GraphicsContext Vector2
getCanvasSize = do
  ctx <- S.get
  size <- S.lift $ GC.getCanvasDimensions $ canvas ctx
  S.lift $ pure $ size.width <=> size.height

-- | Sets the size of the current canvas.
setCanvasSize :: Vector2 -> CanvasRuntime GraphicsContext Unit
setCanvasSize size = do
  ctx <- S.get
  S.lift $ GC.setCanvasDimensions (canvas ctx) { width: getX size, height: getY size }

{- Styling -}
-- | Sets the current fill style.
setFillStyle :: String -> CanvasRuntime GraphicsContext Unit
setFillStyle style = do
  ctx <- S.get
  S.lift $ GC.setFillStyle (context ctx) style

-- | Gets the current fill style.
getFillStyle :: CanvasRuntime GraphicsContext String
getFillStyle = do
  ctx <- S.get
  S.lift $ getFillStyle_ (context ctx)

-- | Sets the current stroke style.
setStrokeStyle :: String -> CanvasRuntime GraphicsContext Unit
setStrokeStyle style = do
  ctx <- S.get
  S.lift $ GC.setStrokeStyle (context ctx) style

-- | Gets the current stroke style.
getStrokeStyle :: CanvasRuntime GraphicsContext String
getStrokeStyle = do
  ctx <- S.get
  S.lift $ getStrokeStyle_ (context ctx)

-- | Sets the current stroke width.
setStrokeWidth :: Number -> CanvasRuntime GraphicsContext Unit
setStrokeWidth width = do
  ctx <- S.get
  S.lift $ GC.setLineWidth (context ctx) width

-- | Gets the current stroke width.
getStrokeWidth :: CanvasRuntime GraphicsContext Number
getStrokeWidth = do
  ctx <- S.get
  S.lift $ getStrokeWidth_ (context ctx)

{- Transforms -}
-- | Returns the current transform.
getTransform :: CanvasRuntime GraphicsContext GC.Transform
getTransform = do
  ctx <- S.get
  S.lift $ getTransform_ (context ctx)

-- | Sets the current transform.
setTransform :: GC.Transform -> CanvasRuntime GraphicsContext Unit
setTransform transform = do
  ctx <- S.get
  S.lift $ GC.setTransform (context ctx) transform

-- | Resets the current transform to the identity matrix.
resetTransform :: CanvasRuntime GraphicsContext Unit
resetTransform = do
  setTransform { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0 }

-- | Translates the entire canvas.
translate :: Vector2 -> CanvasRuntime GraphicsContext Unit
translate offset = do
  ctx <- S.get
  S.lift $ GC.translate (context ctx) { translateX: getX offset, translateY: getY offset }

-- | Scales the entire canvas.
scale :: Vector2 -> CanvasRuntime GraphicsContext Unit
scale factor = do
  ctx <- S.get
  S.lift $ GC.scale (context ctx) { scaleX: getX factor, scaleY: getY factor }

{- Drawing -}
-- | Draws the entire canvas with the specified style.
background :: String -> CanvasRuntime GraphicsContext Unit
background style = do
  ctx <- S.get
  -- Back-up old style/transform.
  oldTransform <- getTransform
  oldFillStyle <- getFillStyle
  -- Setup canvas.
  resetTransform
  dimensions <- getCanvasSize
  S.lift $ GC.setFillStyle (context ctx) style
  -- Fill.
  fillRect zero dimensions
  -- Revert style/transform.
  setFillStyle oldFillStyle
  setTransform oldTransform

-- | Fills the specified area with the specified style.
fillRect :: Vector2 -> Vector2 -> CanvasRuntime GraphicsContext Unit
fillRect base size = do
  ctx <- S.get
  S.lift $ GC.fillRect (context ctx) { x: getX base, y: getY base, width: getX size, height: getY size }

-- | Draws a circle at the specified location, with the specified size.
ellipse :: Vector2 -> Vector2 -> CanvasRuntime GraphicsContext Unit
ellipse position size = do
  ctx <- S.get
  S.lift $ GC.beginPath $ context ctx
  S.lift $ ellipse_ (context ctx) (getX position) (getY position) (getX size) (getY size) 0.0 0.0 (2.0 * pi) false
  S.lift $ GC.fill $ context ctx
  S.lift $ GC.stroke $ context ctx

-- | Draws a circle at the specified location, with the specified radius.
circle :: Vector2 -> Number -> CanvasRuntime GraphicsContext Unit
circle position radius = ellipse position (diagonal radius)

-- | Draws a line from the specified location to the specified location.
line :: Vector2 -> Vector2 -> CanvasRuntime GraphicsContext Unit
line from to = do
  ctx <- S.get
  S.lift $ GC.beginPath (context ctx)
  S.lift $ GC.moveTo (context ctx) (getX from) (getY from)
  S.lift $ GC.lineTo (context ctx) (getX to) (getY to)
  S.lift $ GC.stroke (context ctx)

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
