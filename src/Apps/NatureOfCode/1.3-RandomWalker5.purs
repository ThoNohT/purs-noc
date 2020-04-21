-- | Implements a random walker, modified like in https://www.youtube.com/watch?v=jupjuq9Jl-M.
-- | This walker walks off of the screen in a random direction, with a random speed.
module Apps.NatureOfCode.RandomWalker5 (app) where

import Prelude
import Data.Maybe (Maybe(..))
import Graphics as G
import Model.Random as Random
import Model.Vector (Vector2, randomVector, (<=>), (|*|), (|/|))
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

type State
  = { pos :: Vector2, vel :: Vector2 }

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: State -> CanvasRuntime (Maybe State)
initialize state = do
  G.setCanvasSize canvasSize
  velocity <- randomVector
  mult <- Random.randomRange 0.0 3.0
  pure $ Just $ state { vel = velocity |*| mult }

tick :: State -> CanvasRuntime (Maybe State)
tick state = do
  pure $ Just $ state { pos = state.pos + state.vel }

render :: State -> CanvasRuntime Unit
render state = do
  G.background "black"
  G.setFillStyle "#FFFFFF64"
  G.setStrokeStyle "white"
  G.setStrokeWidth 2.0
  G.circle state.pos 32.0

app :: CanvasApp State
app =
  (defaultApp { pos: canvasSize |/| 2.0, vel: zero })
    { initialize = initialize
    , render = render
    , tick = tick
    }
