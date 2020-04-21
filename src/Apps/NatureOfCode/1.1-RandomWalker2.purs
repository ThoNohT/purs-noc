-- | Implements a random walker, as the first modification in https://www.youtube.com/watch?v=bKEaK7WNLzM.
module Apps.NatureOfCode.RandomWalker2 (app) where

import Prelude
import Data.Maybe (Maybe(..))
import Graphics as G
import Model.Vector (Vector2, randomVector, (<=>), (|/|))
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

type State
  = Vector2

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: State -> CanvasRuntime (Maybe State)
initialize state = do
  G.setCanvasSize canvasSize
  G.background "black"
  pure Nothing

tick :: State -> CanvasRuntime (Maybe State)
tick state = do
  diff <- randomVector
  pure $ Just $ state + diff

render :: State -> CanvasRuntime Unit
render state = do
  G.setFillStyle "#FFFFFF64"
  G.circle state 4.0

app :: CanvasApp State
app =
  (defaultApp (canvasSize |/| 2.0))
    { initialize = initialize
    , render = render
    , tick = tick
    }
