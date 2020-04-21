-- | Implements a walker, modified like in https://www.youtube.com/watch?v=Rob0pbE7kks.
-- | This walker just walks off of the screen in one direction.
module Apps.NatureOfCode.RandomWalker4 (app) where

import Prelude
import Data.Maybe (Maybe(..))
import Graphics as G
import Model.Vector (Vector2, (<=>), (|/|))
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

type State
  = { pos :: Vector2, vel :: Vector2 }

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: State -> CanvasRuntime (Maybe State)
initialize state = G.setCanvasSize canvasSize >>= const (pure Nothing)

tick :: State -> CanvasRuntime (Maybe State)
tick state = do
  pure $ Just $ state { pos = state.pos + state.vel }

render :: State -> CanvasRuntime Unit
render state = do
  G.background "black"
  G.setFillStyle "#FFFFFF64"
  G.setStrokeStyle "white"
  G.setStrokeWidth 2.0
  G.circle state.pos 32.0

app :: CanvasApp State
app =
  (defaultApp { pos: canvasSize |/| 2.0, vel: 1.0 <=> -1.0 })
    { initialize = initialize
    , render = render
    , tick = tick
    }
