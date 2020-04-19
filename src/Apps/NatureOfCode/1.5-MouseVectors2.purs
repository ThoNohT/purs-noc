-- | Implements the vector pointing at the mouse with normalized length from https://www.youtube.com/watch?v=ttz05d8DSOs.
module Apps.NatureOfCode.MouseVectors2 (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics as G
import Model.Events (MouseEvent(..), MouseData)
import Model.Vector (Vector2, setMagnitude, (<=>), (|/|))

type State
  = Vector2

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: G.GraphicsContext -> State -> Effect (Maybe State)
initialize ctx _ = do
  G.setCanvasSize ctx canvasSize
  G.translate ctx $ canvasSize |/| 2.0
  pure Nothing

tick :: State -> Effect (Maybe State)
tick state = do
  pure Nothing

handleMouse :: MouseData -> State -> Effect (Maybe State)
handleMouse mouse state = case mouse.event of
  MouseMove -> pure $ Just mouse.location
  _ -> pure Nothing

render :: G.GraphicsContext -> State -> Effect Unit
render ctx state = do
  G.background ctx "black"
  let
    v = state - (200.0 <=> 200.0) # setMagnitude 50.0
  G.setStrokeStyle ctx "white"
  G.setStrokeWidth ctx 2.0
  G.line ctx zero v

app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec zero)
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        , handleMouse = handleMouse
        }
