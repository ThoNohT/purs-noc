-- | Implements a walker, modified like in https://www.youtube.com/watch?v=Rob0pbE7kks.
-- | This walker just walks off of the screen in one direction.
module Apps.NatureOfCode.RandomWalker4 (app) where

import Prelude

import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics (background, fillArea, point)
import Graphics.Canvas as GC
import Model.Vector (Vector2, diagonal, (<=>))

-- | The state just contains the location of the mouse.
type State
  = { pos :: Vector2, vel :: Vector2 }

-- | This value is used to set and get the canvas size everywhere  else.
canvasSize :: GC.Dimensions
canvasSize = { width: 400.0, height: 400.0 }

-- | Sets the canvas to the desired size.
initialize :: GC.CanvasElement -> State -> Effect Unit
initialize canvas state = do
  _ <- GC.setCanvasDimensions canvas canvasSize
  background canvas "black"

tick :: State -> Effect (Maybe State)
tick state = do
  pure $ Just $ state { pos = state.pos + state.vel }

-- | Renders a white background, and a red square around the mouse position.
render :: GC.Context2D -> State -> Effect Unit
render ctx state = do
  _ <- fillArea ctx zero (canvasSize.width <=> canvasSize.height) "black"
  _ <- GC.setFillStyle ctx "#FFFFFF64"
  _ <- GC.setStrokeStyle ctx "white"
  _ <- GC.setLineWidth ctx 2.0
  point ctx state.pos 16.0

-- | Define the main application.
app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec { pos: diagonal $ canvasSize.width / 2.0, vel: 1.0 <=> -1.0 })
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        }
