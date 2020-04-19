-- | Implements a random walker, as the first modification in https://www.youtube.com/watch?v=bKEaK7WNLzM.
module Apps.NatureOfCode.RandomWalker2 (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics (background, point)
import Graphics.Canvas as GC
import Model.Vector (Vector2, randomVector, (<=>))

-- | The state just contains the location of the mouse.
type State
  = Vector2

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
  diff <- randomVector
  pure $ Just $ state + diff

-- | Renders a white background, and a red square around the mouse position.
render :: GC.Context2D -> State -> Effect Unit
render ctx state = do
  _ <- GC.setFillStyle ctx "#FFFFFF64"
  point ctx state 2.0

-- | Define the main application.
app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec ((canvasSize.width / 2.0) <=> (canvasSize.height / 2.0)))
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        }
