-- | Implements random walker that uses position, velocity and accelleration, as shown in
-- | https://www.youtube.com/watch?v=T84AWnntxZA.
module Apps.NatureOfCode.RandomWalker6 (app) where

import Prelude
import Data.Maybe (Maybe(..))
import Graphics as G
import Model.Random as Random
import Model.Vector (Vector2, limit, randomVector, (<=>), (|*|), (|/|))
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

type State
  = { pos :: Vector2, vel :: Vector2 }

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: State -> CanvasRuntime (Maybe State)
initialize state = do
  G.setCanvasSize canvasSize
  velocity <- randomVector
  velFactor <- Random.randomRange 0.0 3.0
  pure $ Just $ state { vel = velocity |*| velFactor }

tick :: State -> CanvasRuntime (Maybe State)
tick state = do
  acc <- randomVector
  let
    newVel = state.vel + acc # limit 2.0

    newPos = state.pos + newVel
  pure $ Just $ state { pos = newPos, vel = newVel }

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
