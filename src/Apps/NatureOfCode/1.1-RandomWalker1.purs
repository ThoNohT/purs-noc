-- | Implements a random walker, as from the beginning of https://www.youtube.com/watch?v=bKEaK7WNLzM.
-- | Except since I am using state, the 'Vector' class here is already a point.
module Apps.NatureOfCode.RandomWalker1 (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Random as Random
import Graphics as G
import Model.Vector (Vector2, (<=>), (|/|))

type State
  = Vector2

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: G.GraphicsContext -> State -> Effect (Maybe State)
initialize ctx state = do
  G.setCanvasSize ctx canvasSize
  G.background ctx "black"
  pure Nothing

tick :: State -> Effect (Maybe State)
tick state = do
  val <- Random.randomInt 0 3
  let
    newState = case { val: val } of
      { val: 0 } -> state + (1.0 <=> 0.0)
      { val: 1 } -> state - (1.0 <=> 0.0)
      { val: 2 } -> state + (0.0 <=> 1.0)
      { val: _ } -> state - (0.0 <=> 1.0)
  pure $ Just newState

render :: G.GraphicsContext -> State -> Effect Unit
render ctx state = do
  G.setFillStyle ctx "#FFFFFF64"
  G.point ctx state 2.0

app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec (canvasSize |/| 2.0))
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        }
