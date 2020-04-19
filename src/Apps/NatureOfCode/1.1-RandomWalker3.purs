-- | Implements a random walker, as the second modification in https://www.youtube.com/watch?v=bKEaK7WNLzM.
-- | Except for using a class, this walker is a module. I'm also not sure if I will be continuing this style in the
-- | Future, as this is not object oriented programming.
module Apps.NatureOfCode.RandomWalker3 (app) where

import Prelude

import App as App
import Apps.NatureOfCode.Randomwalker3.Walker as Walker
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics (background)
import Graphics.Canvas as GC

-- | The state just contains the location of the mouse.
type State
  = Walker.Walker

-- | This value is used to set and get the canvas size everywhere  else.
canvasSize :: GC.Dimensions
canvasSize = { width: 400.0, height: 400.0 }

-- | Sets the canvas to the desired size.
initialize :: GC.CanvasElement -> State -> Effect (Maybe State)
initialize canvas state = do
  _ <- GC.setCanvasDimensions canvas canvasSize
  _ <- background canvas "black"
  pure Nothing

tick :: State -> Effect (Maybe State)
tick state = do
  Just <$> Walker.update state

-- | Renders a white background, and a red square around the mouse position.
render :: GC.Context2D -> State -> Effect Unit
render ctx state = Walker.render ctx state

-- | Define the main application.
app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec $ Walker.init canvasSize)
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        }
