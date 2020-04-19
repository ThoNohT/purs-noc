-- | Implements a walker, modified like in https://www.youtube.com/watch?v=Rob0pbE7kks.
-- | This walker just walks off of the screen in one direction.
module Apps.NatureOfCode.RandomWalker4 (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics as G
import Model.Vector (Vector2, (<=>), (|/|))

type State
  = { pos :: Vector2, vel :: Vector2 }

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: G.GraphicsContext -> State -> Effect (Maybe State)
initialize ctx state = G.setCanvasSize ctx canvasSize >>= const (pure Nothing)

tick :: State -> Effect (Maybe State)
tick state = do
  pure $ Just $ state { pos = state.pos + state.vel }

render :: G.GraphicsContext -> State -> Effect Unit
render ctx state = do
  _ <- G.background ctx "black"
  _ <- G.setFillStyle ctx "#FFFFFF64"
  _ <- G.setStrokeStyle ctx "white"
  _ <- G.setStrokeWidth ctx 2.0
  G.point ctx state.pos 16.0

app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec { pos: canvasSize |/| 2.0, vel: 1.0 <=> -1.0 })
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        }
