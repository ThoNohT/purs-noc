module Apps.Village.Village (app) where

import Prelude
import Apps.Village.Model (World)
import Apps.Village.World as World
import Data.Int (rem) as Math
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Framework.Types (liftEffect)
import Graphics as G
import Model.Vector (Vector2, (<=>))
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

-- | The state just contains the location of the mouse.
type State
  = { world :: World, tickCount :: Int }

-- | This value is used to set and get the canvas size everywhere else.
canvasSize :: Vector2
canvasSize = 600.0 <=> 600.0

-- | Sets the canvas to the desired size.
initialize :: State -> CanvasRuntime (Maybe State)
initialize state = do
  G.setCanvasSize canvasSize
  G.background "black"
  pure Nothing

-- | Renders a white background, and a red square around the mouse position.
render :: State -> CanvasRuntime Unit
render state = do
  World.render state.world

tickOrUpdate :: State -> (Unit -> Effect State) -> (Unit -> Effect State) -> CanvasRuntime (Maybe State)
tickOrUpdate state onUpdate onTick =
  liftEffect $ Just
    <$> do
        updatedState <- case state.tickCount == 0 of
          true -> onUpdate unit
          false -> onTick unit
        pure $ updatedState { tickCount = Math.rem (state.tickCount + 1) 10 }

tick :: State -> CanvasRuntime (Maybe State)
tick state =
  tickOrUpdate state
    ( \_ -> do
        newWorld <- World.update state.world
        pure $ state { world = newWorld }
    )
    ( \_ -> do
        newWorld <- World.tick state.world
        pure $ state { world = newWorld }
    )

-- | Define the main application.
app :: CanvasApp State
app =
  (defaultApp { world: World.init canvasSize, tickCount: 0 })
    { initialize = initialize
    , tick = tick
    , render = render
    , updateInterval = 10
    }
