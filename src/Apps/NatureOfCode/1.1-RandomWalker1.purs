-- | Implements a random walker, as from the beginning of https://www.youtube.com/watch?v=bKEaK7WNLzM.
-- | Except since I am using state, the 'Vector' class here is already a point.
module Apps.NatureOfCode.RandomWalker1 (app) where

import Prelude
import Control.Monad.State.Trans as S
import Data.Maybe (Maybe(..))
import Effect.Random as Random
import Graphics as G
import Model.Vector (Vector2, (<=>), (|/|))
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
  val <- S.lift $ Random.randomInt 0 3
  let
    newState = case { val: val } of
      { val: 0 } -> state + (1.0 <=> 0.0)
      { val: 1 } -> state - (1.0 <=> 0.0)
      { val: 2 } -> state + (0.0 <=> 1.0)
      { val: _ } -> state - (0.0 <=> 1.0)
  pure $ Just newState

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
