-- | Implements a random walker, as the first modification in https://www.youtube.com/watch?v=bKEaK7WNLzM.
module Apps.NatureOfCode.RandomWalker2 (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics as G
import Model.Vector (Vector2, randomVector, (<=>), (|/|))

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
  diff <- randomVector
  pure $ Just $ state + diff

render :: G.GraphicsContext -> State -> Effect Unit
render ctx state = do
  G.setFillStyle ctx "#FFFFFF64"
  G.circle ctx state 4.0

app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec (canvasSize |/| 2.0))
        { initialize = initialize
        , render = render
        , tick = tick
        }
