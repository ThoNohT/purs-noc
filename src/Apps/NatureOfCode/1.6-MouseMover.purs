-- | Implements mover that accellerates towards the mouse, shown in https://www.youtube.com/watch?v=T84AWnntxZA.
module Apps.NatureOfCode.MouseMover (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics as G
import Model.Events (MouseEvent(..), MouseData)
import Model.Vector (Vector2, limit, setMagnitude, (<=>), (|/|))

-- | The state just contains the location of the mouse.
type State
  = { pos :: Vector2, vel :: Vector2, mousePos :: Vector2 }

-- | This value is used to set and get the canvas size everywhere  else.
canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

center :: Vector2
center = canvasSize |/| 2.0

-- | Sets the canvas to the desired size.
initialize :: G.GraphicsContext -> State -> Effect (Maybe State)
initialize ctx state = do
  _ <- G.setCanvasSize ctx canvasSize
  pure Nothing

tick :: State -> Effect (Maybe State)
tick state = do
  let
    acc = state.mousePos - state.pos # setMagnitude 1.0

    newVel = state.vel + acc # limit 25.0

    newPos = state.pos + newVel
  pure $ Just $ state { pos = newPos, vel = newVel }

handleMouse :: MouseData -> State -> Effect (Maybe State)
handleMouse event state = case event.event of
  MouseMove -> pure $ Just $ state { mousePos = event.location }
  _ -> pure Nothing

-- | Renders a white background, and a red square around the mouse position.
render :: G.GraphicsContext -> State -> Effect Unit
render ctx state = do
  _ <- G.background ctx "black"
  _ <- G.setFillStyle ctx "#FFFFFF64"
  _ <- G.setStrokeStyle ctx "white"
  _ <- G.setStrokeWidth ctx 2.0
  G.point ctx state.pos 16.0

-- | Define the main application.
app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec { pos: center, vel: zero, mousePos: center })
        { initialize = initialize
        , render = render
        , tick = tick
        , handleMouse = handleMouse
        }
