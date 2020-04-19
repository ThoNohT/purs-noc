-- | Implements the vectors pointing at the mouse from https://www.youtube.com/watch?v=ttz05d8DSOs.
module Apps.NatureOfCode.MouseVectors1 (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics as G
import Model.Events (MouseEvent(..), MouseData)
import Model.Vector (Vector2, (<=>), (|/|))

type State
  = Vector2

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: G.GraphicsContext -> State -> Effect (Maybe State)
initialize ctx _ = do
  _ <- G.setCanvasSize ctx canvasSize
  _ <- G.background ctx "black"
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
  _ <- G.resetTransform ctx
  _ <- G.translate ctx $ canvasSize |/| 2.0
  let
    v = state - (200.0 <=> 200.0)
  _ <- G.setStrokeStyle ctx "#FFFFFF64"
  _ <- G.setStrokeWidth ctx 2.0
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
