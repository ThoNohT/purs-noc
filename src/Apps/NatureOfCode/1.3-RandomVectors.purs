-- | Implements the random vectors from https://www.youtube.com/watch?v=jupjuq9Jl-M.
module Apps.NatureOfCode.RandomVectors (app) where

import Prelude
import Data.Maybe (Maybe(..))
import Graphics as G
import Model.Vector (Vector2, randomVector, (|*|), (|/|), (<=>))
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

type State
  = Vector2

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: State -> CanvasRuntime (Maybe State)
initialize _ = do
  G.setCanvasSize canvasSize
  G.background "black"
  G.translate $ canvasSize |/| 2.0
  pure Nothing

tick :: State -> CanvasRuntime (Maybe State)
tick state = do
  v <- randomVector
  pure $ Just $ v |*| 100.0

render :: State -> CanvasRuntime Unit
render state = do
  G.setStrokeStyle "white"
  G.setStrokeWidth 4.0
  G.line zero state

app :: CanvasApp State
app =
  (defaultApp zero)
    { initialize = initialize
    , render = render
    , tick = tick
    }
