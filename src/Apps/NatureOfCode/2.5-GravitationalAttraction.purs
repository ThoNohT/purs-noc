-- | Implements mover that experiences gravitational attraction as shown in https://www.youtube.com/watch?v=EpgB3cNhKPM.
module Apps.NatureOfCode.GravitationalAttraction (app) where

import Prelude
import Data.List (List(..), (..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Graphics as G
import Math (sqrt) as Math
import Model.Events (MouseData, MouseEvent(..))
import Model.Math (constrain) as Math
import Model.Random as Random
import Model.Vector (Vector2, getX, getY, magSqr, randomVector, setMagnitude, (<=>), (|*|), (|/|))
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

type State
  = { movers :: List Mover, attractor :: Attractor, mouseIsDown :: Boolean, mousePos :: Vector2 }

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

center :: Vector2
center = canvasSize |/| 2.0

initialize :: State -> CanvasRuntime (Maybe State)
initialize state =
  let
    randomMover _ = do
      posX <- Random.randomRange 1.0 (getX canvasSize)
      posY <- Random.randomRange 1.0 (getY canvasSize)
      vel <- randomVector
      mass <- Random.randomRange 1.0 8.0
      pure $ makeMover (posX <=> posY) (vel |*| 5.0) mass
  in
    do
      G.setCanvasSize canvasSize
      movers <- sequence $ (1 .. 10) # map randomMover
      pure $ Just $ state { movers = movers }

tick :: State -> CanvasRuntime (Maybe State)
tick state =
  let
    attractorPos = if state.mouseIsDown then state.mousePos else state.attractor.pos
  in
    pure $ Just
      $ state
          { movers = map (updateMover state.attractor) state.movers
          , attractor = state.attractor { pos = attractorPos }
          }

handleMouse :: MouseData -> State -> CanvasRuntime (Maybe State)
handleMouse event state = case event.event of
  MouseMove -> pure $ if state.mouseIsDown then Just $ state { mousePos = event.location } else Nothing
  MouseDown -> pure $ Just $ state { mouseIsDown = true, mousePos = event.location }
  MouseUp -> pure $ Just $ state { mouseIsDown = false }

render :: State -> CanvasRuntime Unit
render state = do
  G.background "black"
  _ <- sequence $ map (renderMover) state.movers
  renderAttractor state.attractor

app :: CanvasApp State
app =
  (defaultApp { movers: Nil, attractor: makeAttractor center 5.0, mouseIsDown: false, mousePos: center })
    { initialize = initialize
    , render = render
    , tick = tick
    , handleMouse = handleMouse
    }

-------------------- Mover --------------------
type Mover
  = { pos :: Vector2, vel :: Vector2, mass :: Number, radius :: Number }

makeMover :: Vector2 -> Vector2 -> Number -> Mover
makeMover pos vel mass = { pos: pos, vel: vel, mass: mass, radius: 10.0 * Math.sqrt mass }

renderMover :: Mover -> CanvasRuntime Unit
renderMover mover = do
  G.setFillStyle "#FFFFFF64"
  G.setStrokeStyle "white"
  G.setStrokeWidth 2.0
  G.circle mover.pos mover.radius

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

updateMover :: Attractor -> Mover -> Mover
updateMover attractor mover =
  let
    applyForce :: Vector2 -> Vector2 -> Vector2
    applyForce force acceleration = acceleration + (force |/| mover.mass)

    applyAcc a m = m { vel = m.vel + a }

    applyVel m = m { pos = m.pos + m.vel }

    acc = attract attractor mover
  in
    mover # applyAcc acc # applyVel

-------------------- Attractor --------------------
type Attractor
  = { pos :: Vector2, mass :: Number, radius :: Number }

makeAttractor :: Vector2 -> Number -> Attractor
makeAttractor pos mass = { pos: pos, mass: mass, radius: 10.0 * Math.sqrt mass }

renderAttractor :: Attractor -> CanvasRuntime Unit
renderAttractor attractor = do
  G.setFillStyle "red"
  G.setStrokeStyle "white"
  G.setStrokeWidth 2.0
  G.circle attractor.pos attractor.radius

attract :: Attractor -> Mover -> Vector2
attract attractor mover =
  let
    force = attractor.pos - mover.pos

    distSq = magSqr force # Math.constrain 100.0 1000.0

    g = 5.0

    strength = (g * attractor.mass * mover.mass) / distSq
  in
    setMagnitude strength force
