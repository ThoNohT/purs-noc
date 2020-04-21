-- | Implements mover that accellerates towards the mouse, shown in https://www.youtube.com/watch?v=T84AWnntxZA.
module Apps.NatureOfCode.MouseMover (app) where

import Prelude
import Data.Maybe (Maybe(..))
import Graphics as G
import Model.Events (MouseEvent(..), MouseData)
import Model.Vector (Vector2, limit, setMagnitude, (<=>), (|/|))
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

type State
  = { pos :: Vector2, vel :: Vector2, mousePos :: Vector2 }

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

center :: Vector2
center = canvasSize |/| 2.0

initialize :: State -> CanvasRuntime (Maybe State)
initialize state = G.setCanvasSize canvasSize >>= const (pure Nothing)

tick :: State -> CanvasRuntime (Maybe State)
tick state = do
  let
    acc = state.mousePos - state.pos # setMagnitude 1.0

    newVel = state.vel + acc # limit 25.0

    newPos = state.pos + newVel
  pure $ Just $ state { pos = newPos, vel = newVel }

handleMouse :: MouseData -> State -> CanvasRuntime (Maybe State)
handleMouse event state = case event.event of
  MouseMove -> pure $ Just $ state { mousePos = event.location }
  _ -> pure Nothing

render :: State -> CanvasRuntime Unit
render state = do
  G.background "black"
  G.setFillStyle "#FFFFFF64"
  G.setStrokeStyle "white"
  G.setStrokeWidth 2.0
  G.circle state.pos 32.0

app :: CanvasApp State
app =
  (defaultApp { pos: center, vel: zero, mousePos: center })
    { initialize = initialize
    , render = render
    , tick = tick
    , handleMouse = handleMouse
    }
