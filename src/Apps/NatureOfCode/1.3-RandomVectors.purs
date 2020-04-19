-- | Implements the random vectors from https://www.youtube.com/watch?v=jupjuq9Jl-M.
module Apps.NatureOfCode.RandomVectors (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics as G
import Model.Vector (Vector2, randomVector, (|*|), (|/|), (<=>))

type State
  = Vector2

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: G.GraphicsContext -> State -> Effect (Maybe State)
initialize ctx _ = do
  _ <- G.setCanvasSize ctx canvasSize
  _ <- G.background ctx "black"
  _ <- G.translate ctx $ canvasSize |/| 2.0
  pure Nothing

tick :: State -> Effect (Maybe State)
tick state = do
  v <- randomVector
  pure $ Just $ v |*| 100.0

render :: G.GraphicsContext -> State -> Effect Unit
render ctx state = do
  _ <- G.setStrokeStyle ctx "white"
  _ <- G.setStrokeWidth ctx 4.0
  G.line ctx zero state

app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec zero)
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        }
