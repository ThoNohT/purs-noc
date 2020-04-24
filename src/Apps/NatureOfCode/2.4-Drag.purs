-- | Implements mover that has drag added to the forces being applied to it as shown in https://www.youtube.com/watch?v=DxFDgOYEoy8.
module Apps.NatureOfCode.Drag (app) where

import Prelude
import Data.List (List(..), (..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Graphics as G
import Math as Math
import Model.Events (MouseData, MouseEvent(..))
import Model.Random as Random
import Model.Vector (Vector2, getX, getY, magSqr, normalize, scale, setMagnitude, setX, setY, (<=>), (|*|), (|/|))
import Prelude as List
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

type State
  = { movers :: List Mover, mouseIsDown :: Boolean }

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: State -> CanvasRuntime (Maybe State)
initialize state =
  let
    mover pos mass = { pos: pos <=> 0.0, vel: zero, mass: mass, radius: 10.0 * Math.sqrt mass }

    randomMover _ = do
      x <- Random.randomRange 0.0 (getX canvasSize)
      mass <- Random.randomRange 1.0 8.0
      pure $ mover x mass
  in
    do
      G.setCanvasSize canvasSize
      movers <- sequence $ (1 .. 10) # List.map randomMover
      pure $ Just $ state { movers = movers }

tick :: State -> CanvasRuntime (Maybe State)
tick state =
  let
    gravity = 0.0 <=> 0.1

    wind = if state.mouseIsDown then 0.1 <=> 0.0 else zero
  in
    pure $ Just $ state { movers = map (updateMover gravity wind) state.movers }

handleMouse :: MouseData -> State -> CanvasRuntime (Maybe State)
handleMouse event state = case event.event of
  MouseDown -> pure $ Just state { mouseIsDown = true }
  MouseUp -> pure $ Just state { mouseIsDown = false }
  _ -> pure Nothing

render :: State -> CanvasRuntime Unit
render state = do
  G.background "black"
  G.setFillStyle "#FFFFFF7D"
  let
    halfHeight = (getY canvasSize) / 2.0
  G.fillRect (0.0 <=> halfHeight) (getX canvasSize <=> halfHeight)
  _ <- sequence $ map (renderMover) state.movers
  pure unit

app :: CanvasApp State
app =
  (defaultApp { movers: Nil, mouseIsDown: false })
    { initialize = initialize
    , render = render
    , tick = tick
    , handleMouse = handleMouse
    , updateInterval = 10
    }

-------------------- Mover --------------------
type Mover
  = { pos :: Vector2, vel :: Vector2, mass :: Number, radius :: Number }

renderMover :: Mover -> CanvasRuntime Unit
renderMover mover = do
  G.setFillStyle "#FFFFFF64"
  G.setStrokeStyle "white"
  G.setStrokeWidth 2.0
  G.circle mover.pos mover.radius

-- | Applies the a force to an accelleration vector.
applyForce :: Vector2 -> Number -> Vector2 -> Vector2
applyForce force mass acceleration = acceleration + force |/| mass

-- | Handles a single edge.
edge ::
  (Vector2 -> Number) ->
  (Number -> Vector2 -> Vector2) ->
  Number ->
  Number ->
  (Number -> Number -> Boolean) ->
  Vector2 ->
  Mover -> Mover
edge getPosComponent setPosComponent bound offset compare velFlipVector mover =
  if getPosComponent mover.pos `compare` (bound - offset) then
    mover
      { pos = setPosComponent (bound - offset) mover.pos
      , vel = mover.vel * velFlipVector
      }
  else
    mover

-- | Ensures the object doesn't cross the left/right/bottom edges.
edges :: Mover -> Mover
edges mover =
  mover
    # edge getY setY 400.0 mover.radius (>=) (1.0 <=> -1.0)
    # edge getX setX 400.0 mover.radius (>=) (-1.0 <=> 1.0)
    # edge getX setX 0.0 (-mover.radius) (<=) (-1.0 <=> 1.0)

updateMover :: Vector2 -> Vector2 -> Mover -> Mover
updateMover gravity wind mover =
  let
    friction = calculateFriction mover

    drag = calculateDrag mover

    applyAcc a m = m { vel = m.vel + a }

    applyVel m = m { pos = m.pos + m.vel }

    acc =
      zero
        # applyForce drag mover.mass
        # applyForce friction mover.mass
        # applyForce (gravity |*| mover.mass) mover.mass
        # applyForce wind mover.mass
  in
    mover # applyAcc acc # applyVel # edges

calculateFriction :: Mover -> Vector2
calculateFriction mover =
  let
    diff = getY canvasSize - (getY mover.pos + mover.radius)
  in
    if diff < 1.0 then
      let
        mu = 0.1

        normal = mover.mass
      in
        mover.vel
          # normalize
          # scale (-1.0)
          # setMagnitude (mu * normal)
    else
      zero

calculateDrag :: Mover -> Vector2
calculateDrag mover =
  let
    c = if (getY mover.pos) > (getY canvasSize) / 2.0 then 0.2 else -0.0

    speedSqr = magSqr mover.vel
  in
    mover.vel
      # normalize
      # scale (-1.0)
      # setMagnitude (c * speedSqr)
