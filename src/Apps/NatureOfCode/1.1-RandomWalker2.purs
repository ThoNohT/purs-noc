-- | Implements a random walker, as the first modification in https://www.youtube.com/watch?v=bKEaK7WNLzM.
module Apps.NatureOfCode.RandomWalker2 (app) where

import Prelude
import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics as G
import Model.Vector (Vector2, randomVector, (<=>), (|/|))

-- | The state just contains the location of the mouse.
type State
  = Vector2

-- | This value is used to set and get the canvas size everywhere  else.
canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

-- | Sets the canvas to the desired size.
initialize :: G.GraphicsContext -> State -> Effect (Maybe State)
initialize ctx state = do
  _ <- G.setCanvasSize ctx canvasSize
  _ <- G.background ctx "black"
  pure Nothing

tick :: State -> Effect (Maybe State)
tick state = do
  diff <- randomVector
  pure $ Just $ state + diff

-- | Renders a white background, and a red square around the mouse position.
render :: G.GraphicsContext -> State -> Effect Unit
render ctx state = do
  _ <- G.setFillStyle ctx "#FFFFFF64"
  G.point ctx state 2.0

-- | Define the main application.
app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec (canvasSize |/| 2.0))
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        }
