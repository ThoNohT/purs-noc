-- | Implements the random vectors from https://www.youtube.com/watch?v=jupjuq9Jl-M.
module Apps.NatureOfCode.RandomVectors (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics as G
import Model.Vector (Vector2, randomVector, (|*|), (|/|), (<=>))

-- | The state just contains the location of the mouse.
type State
  = Vector2

-- | This value is used to set and get the canvas size everywhere  else.
canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

-- | Sets the canvas to the desired size.
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

-- | Renders a white background, and a red square around the mouse position.
render :: G.GraphicsContext -> State -> Effect Unit
render ctx state = do
  _ <- G.setStrokeStyle ctx "white"
  _ <- G.setStrokeWidth ctx 4.0
  G.line ctx zero state

-- | Define the main application.
app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec zero)
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        }
