-- | Implements a random walker, as from the beginning of https://www.youtube.com/watch?v=bKEaK7WNLzM.
-- | Except since I am using state, the 'Vector' class here is already a point.
module Apps.RandomWalker1 (app) where

import Prelude

import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas as GC
import Model (Point, Interval)
import Effect.Random as Random
import Graphics (background, point)

-- | The state just contains the location of the mouse.
type State
  = Point

-- | This value is used to set and get the canvas size everywhere  else.
canvasSize :: GC.Dimensions
canvasSize = { width: 400.0, height: 400.0 }

-- | Sets the canvas to the desired size.
initialize :: GC.CanvasElement -> State -> Effect Unit
initialize canvas state = do
  _ <- GC.setCanvasDimensions canvas canvasSize
  background canvas "black"

tick :: Interval -> State -> Effect (Maybe State)
tick _ state = do
    val <- Random.randomInt 0 3
    let newState = case { val: val } of
                    { val: 0 } -> state { x = state.x + 1.0 }
                    { val: 1 } -> state { x = state.x - 1.0 }
                    { val: 2 } -> state { y = state.y + 1.0 }
                    { val: _ } -> state { y = state.y - 1.0 }
    pure $ Just newState

-- | Renders a white background, and a red square around the mouse position.
render :: GC.Context2D -> State -> Effect Unit
render ctx state = do
  _ <- GC.setFillStyle ctx "#FFFFFF64"
  point ctx state 2.0

-- | Define the main application.
app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec { x: canvasSize.width / 2.0, y: canvasSize.height / 2.0 })
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        }
