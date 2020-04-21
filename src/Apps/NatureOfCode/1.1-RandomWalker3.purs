-- | Implements a random walker, as the second modification in https://www.youtube.com/watch?v=bKEaK7WNLzM.
-- | Except for using a class, this walker is a module. I'm also not sure if I will be continuing this style in the
-- | Future, as this is not object oriented programming.
module Apps.NatureOfCode.RandomWalker3 (app) where

import Prelude
import Apps.NatureOfCode.Randomwalker3.Walker as Walker
import Data.Maybe (Maybe(..))
import Graphics as G
import Model.Vector (Vector2, (<=>))
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

type State
  = Walker.Walker

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: State -> CanvasRuntime (Maybe State)
initialize state = do
  G.setCanvasSize canvasSize
  G.background "black"
  pure Nothing

tick :: State -> CanvasRuntime (Maybe State)
tick state = do
  Just <$> Walker.update state

render :: State -> CanvasRuntime Unit
render state = Walker.render state

app :: CanvasApp State
app =
  (defaultApp $ Walker.init canvasSize)
    { initialize = initialize
    , render = render
    , tick = tick
    }
