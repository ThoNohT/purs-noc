-- | Implements mover that has forces applied to it as shown in https://www.youtube.com/watch?v=Uibl0UE4VH8.
module Apps.NatureOfCode.ApplyForce (app) where

import Prelude
import Data.Maybe (Maybe(..))
import Graphics as G
import Model.Events (MouseData, MouseEvent(..))
import Model.Vector (Vector2, getX, getY, setX, setY, (<=>), (|/|))
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

type State
  = { mover :: Mover, mouseIsDown :: Boolean }

type Mover
  = { pos :: Vector2, vel :: Vector2 }

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

center :: Vector2
center = canvasSize |/| 2.0

initialize :: State -> CanvasRuntime (Maybe State)
initialize state = G.setCanvasSize canvasSize >>= const (pure Nothing)

-- | Applies the a force to an accelleration vector, given that all mass is 1.
applyForce :: Vector2 -> Vector2 -> Vector2
applyForce force acceleration = force + acceleration

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
    # edge getY setY 400.0 16.0 (>=) (1.0 <=> -1.0)
    # edge getX setX 400.0 16.0 (>=) (-1.0 <=> 1.0)
    # edge getX setX 0.0 (-16.0) (<=) (-1.0 <=> 1.0)

updateMover :: Mover -> Vector2 -> Vector2 -> Mover
updateMover mover gravity wind =
  let
    applyAcc a m = m { vel = m.vel + a }

    applyVel m = m { pos = m.pos + m.vel }

    acc = zero # applyForce gravity # applyForce wind
  in
    mover # applyAcc acc # applyVel # edges

tick :: State -> CanvasRuntime (Maybe State)
tick state =
  let
    gravity = 0.0 <=> 1.6

    wind = if state.mouseIsDown then 1.0 <=> 0.0 else zero
  in
    pure $ Just $ state { mover = updateMover state.mover gravity wind }

handleMouse :: MouseData -> State -> CanvasRuntime (Maybe State)
handleMouse event state = case event.event of
  MouseDown -> pure $ Just state { mouseIsDown = true }
  MouseUp -> pure $ Just state { mouseIsDown = false }
  _ -> pure Nothing

render :: State -> CanvasRuntime Unit
render state = do
  G.background "black"
  G.setFillStyle "#FFFFFF64"
  G.setStrokeStyle "white"
  G.setStrokeWidth 2.0
  G.circle state.mover.pos 32.0

app :: CanvasApp State
app =
  (defaultApp { mover: { pos: center, vel: zero }, mouseIsDown: false })
    { initialize = initialize
    , render = render
    , tick = tick
    , handleMouse = handleMouse
    }
