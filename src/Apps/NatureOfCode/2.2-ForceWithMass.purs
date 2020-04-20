-- | Implements mover that has forces applied to it with mass as shown in https://www.youtube.com/watch?v=L7CECWLdTmo.
module Apps.NatureOfCode.ForceWithMass (app) where

import Prelude
import Data.List (List(..), (:))
import App as App
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Graphics as G
import Math as Math
import Model.Events (MouseData, MouseEvent(..))
import Model.Vector (Vector2, getX, getY, setX, setY, (<=>), (|/|), (|*|))

type State
  = { movers :: List Mover, mouseIsDown :: Boolean }

type Mover
  = { pos :: Vector2, vel :: Vector2, mass :: Number, radius :: Number }

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: G.GraphicsContext -> State -> Effect (Maybe State)
initialize ctx state = G.setCanvasSize ctx canvasSize >>= const (pure Nothing)

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

tick :: State -> Effect (Maybe State)
tick state =
  let
    gravity = 0.0 <=> 1.6

    wind = if state.mouseIsDown then 1.0 <=> 0.0 else zero
  in
    pure $ Just $ state { movers = map (updateMover gravity wind) state.movers }

handleMouse :: MouseData -> State -> Effect (Maybe State)
handleMouse event state = case event.event of
  MouseDown -> pure $ Just state { mouseIsDown = true }
  MouseUp -> pure $ Just state { mouseIsDown = false }
  _ -> pure Nothing

renderMover :: G.GraphicsContext -> Mover -> Effect Unit
renderMover ctx mover = do
  G.setFillStyle ctx "#FFFFFF64"
  G.setStrokeStyle ctx "white"
  G.setStrokeWidth ctx 2.0
  G.circle ctx mover.pos mover.radius

render :: G.GraphicsContext -> State -> Effect Unit
render ctx state = do
  G.background ctx "black"
  _ <- sequence $ map (renderMover ctx) state.movers
  pure unit

app :: App.CanvasApp
app =
  let
    mover pos mass = { pos: pos, vel: zero, mass: mass, radius: 10.0 * Math.sqrt mass }
  in
    App.app
      $ (App.defaultAppSpec { movers: ((mover (100.0 <=> 50.0) 2.0) : (mover (300.0 <=> 50.0) 4.0) : Nil), mouseIsDown: false })
          { initialize = initialize
          , render = render
          , tick = tick
          , handleMouse = handleMouse
          }
