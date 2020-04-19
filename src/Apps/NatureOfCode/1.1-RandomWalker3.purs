-- | Implements a random walker, as the second modification in https://www.youtube.com/watch?v=bKEaK7WNLzM.
-- | Except for using a class, this walker is a module. I'm also not sure if I will be continuing this style in the
-- | Future, as this is not object oriented programming.
module Apps.NatureOfCode.RandomWalker3 (app) where

import Prelude
import App as App
import Apps.NatureOfCode.Randomwalker3.Walker as Walker
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics as G
import Model.Vector (Vector2, (<=>))

type State
  = Walker.Walker

canvasSize :: Vector2
canvasSize = 400.0 <=> 400.0

initialize :: G.GraphicsContext -> State -> Effect (Maybe State)
initialize ctx state = do
  _ <- G.setCanvasSize ctx canvasSize
  _ <- G.background ctx "black"
  pure Nothing

tick :: State -> Effect (Maybe State)
tick state = do
  Just <$> Walker.update state

render :: G.GraphicsContext -> State -> Effect Unit
render ctx state = Walker.render ctx state

app :: App.CanvasApp
app =
  App.app
    $ (App.defaultAppSpec $ Walker.init canvasSize)
        { initialize = initialize
        , render = render
        , tick = tick
        , updateInterval = 33
        }
