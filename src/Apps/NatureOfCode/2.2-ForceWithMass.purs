-- | Implements mover that has forces applied to it with mass as shown in https://www.youtube.com/watch?v=L7CECWLdTmo.
module Apps.NatureOfCode.ForceWithMass (app) where

import Prelude
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Graphics as G
import Math as Math
import Model.Events (MouseData, MouseEvent(..))
import Model.Vector (Vector2, getX, getY, setX, setY, (<=>), (|/|), (|*|))
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

type State
  = { movers :: List Mover, mouseIsDown :: Boolean }

type Mover
  = { pos :: Vector2, vel :: Vector2, mass :: Number, radius :: Number }

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: State -> CanvasRuntime (Maybe State)
initialize state = G.setCanvasSize canvasSize >>= const (pure Nothing)

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
    applyAcc a m = m { vel = m.vel + a }

    applyVel m = m { pos = m.pos + m.vel }

    acc = zero # applyForce (gravity |*| mover.mass) mover.mass # applyForce wind mover.mass
  in
    mover # applyAcc acc # applyVel # edges

tick :: State -> CanvasRuntime (Maybe State)
tick state =
  let
    gravity = 0.0 <=> 1.6

    wind = if state.mouseIsDown then 1.0 <=> 0.0 else zero
  in
    pure $ Just $ state { movers = map (updateMover gravity wind) state.movers }

handleMouse :: MouseData -> State -> CanvasRuntime (Maybe State)
handleMouse event state = case event.event of
  MouseDown -> pure $ Just state { mouseIsDown = true }
  MouseUp -> pure $ Just state { mouseIsDown = false }
  _ -> pure Nothing

renderMover :: Mover -> CanvasRuntime Unit
renderMover mover = do
  G.setFillStyle "#FFFFFF64"
  G.setStrokeStyle "white"
  G.setStrokeWidth 2.0
  G.circle mover.pos mover.radius

render :: State -> CanvasRuntime Unit
render state = do
  G.background "black"
  _ <- sequence $ map (renderMover) state.movers
  pure unit

app :: CanvasApp State
app =
  let
    mover pos mass = { pos: pos, vel: zero, mass: mass, radius: 10.0 * Math.sqrt mass }
  in
    (defaultApp { movers: ((mover (100.0 <=> 50.0) 2.0) : (mover (300.0 <=> 50.0) 4.0) : Nil), mouseIsDown: false })
      { initialize = initialize
      , render = render
      , tick = tick
      , handleMouse = handleMouse
      }
