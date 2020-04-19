-- | Implements a random walker, modified like in https://www.youtube.com/watch?v=jupjuq9Jl-M.
-- | This walker walks off of the screen in a random direction, with a random speed.
module Apps.NatureOfCode.RandomWalker5 (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Random as Random
import Graphics as G
import Model.Vector (Vector2, randomVector, (<=>), (|*|), (|/|))

type State
  = { pos :: Vector2, vel :: Vector2 }

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: G.GraphicsContext -> State -> Effect (Maybe State)
initialize ctx state = do
  G.setCanvasSize ctx canvasSize
  velocity <- randomVector
  mult <- Random.randomRange 0.0 3.0
  pure $ Just $ state { vel = velocity |*| mult }

tick :: State -> Effect (Maybe State)
tick state = do
  pure $ Just $ state { pos = state.pos + state.vel }

render :: G.GraphicsContext -> State -> Effect Unit
render ctx state = do
  G.background ctx "black"
  G.setFillStyle ctx "#FFFFFF64"
  G.setStrokeStyle ctx "white"
  G.setStrokeWidth ctx 2.0
  G.point ctx state.pos 16.0

app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec { pos: canvasSize |/| 2.0, vel: zero })
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        }
