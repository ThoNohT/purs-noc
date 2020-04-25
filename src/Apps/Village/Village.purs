module Apps.Village.Village (app, VillagerAction) where

import Prelude
import Data.Array ((:))
import Data.Array as Array
import Data.Foldable (sum)
import Data.Int (rem) as Math
import Data.Maybe (Maybe(..), fromJust)
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..), snd, fst)
import Effect (Effect)
import Effect.Random as R
import Framework.Types (liftEffect)
import Graphics as G
import Math (atan2, cos, sin) as Math
import Model.Math (radians, degrees) as Math
import Model.Vector (Vector2, dotProduct, determinant, getX, getY, magnitude, (<=>), (|*|))
import Partial.Unsafe (unsafePartial)
import Toolkit (CanvasApp, CanvasRuntime, defaultApp)

-- | The state just contains the location of the mouse.
type State
  = { villager :: Villager, tickCount :: Int }

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
  G.background "green"
  renderVillager state.villager

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
        newVillager <- updateVillager state.villager
        pure $ state { villager = newVillager }
    )
    ( \_ -> do
        newVillager <- tickVillager state.villager
        pure $ state { villager = newVillager }
    )

-- | Define the main application.
app :: CanvasApp State
app =
  (defaultApp { villager: { pos: (100.0 <=> 100.0), action: Standing, heading: (1.0 <=> 0.0), goal: Nothing }, tickCount: 0 })
    { initialize = initialize
    , tick = tick
    , render = render
    , updateInterval = 10
    }

-------------------- Villager --------------------
type Villager
  = { pos :: Vector2, heading :: Vector2, action :: VillagerAction, goal :: Maybe Vector2 }

data VillagerAction
  = Standing
  | Walking
  | Turning Boolean

renderVillager :: Villager -> CanvasRuntime Unit
renderVillager villager = do
  G.setFillStyle "#CCCCCC"
  G.setStrokeStyle "white"
  G.setStrokeWidth 2.0
  G.circle villager.pos 10.0
  G.line villager.pos (villager.pos + villager.heading |*| 20.0)
  case villager.goal of
    Nothing -> pure unit
    Just g -> do
      G.setFillStyle "red"
      G.circle g 3.0

tickVillager :: Villager -> Effect Villager
tickVillager villager =
  pure
    $ case villager.action of
        Standing -> villager
        Walking -> villager { pos = villager.pos + villager.heading }
        Turning left -> villager { heading = rotate (if left then -1.0 else 1.0) villager.heading }

updateVillager :: Villager -> Effect Villager
updateVillager villager = case villager.goal of
  Nothing -> idle villager
  Just g -> pursueGoal g villager

idle :: Villager -> Effect Villager
idle villager =
  let
    pickGoal v = do
      x <- R.randomRange 0.0 (getX canvasSize)
      y <- R.randomRange 0.0 (getY canvasSize)
      pure (v { goal = Just $ x <=> y })
  in
    pick
      [ Tuple 1 pickGoal
      , Tuple 9 (\v -> pure v)
      ]
      villager

pursueGoal :: Vector2 -> Villager -> Effect Villager
pursueGoal g villager =
  let
    align = \a ->
      let
        diff = g - a.pos

        ang = angle a.heading diff
      in
        if ang < -10.0 then
          Just $ \_ -> pure $ a { action = Turning true }
        else
          if ang > 10.0 then
            Just $ \_ -> pure $ a { action = Turning false }
          else
            Nothing

    moveTowards = \a ->
      let
        dist = magnitude (g - a.pos)
      in
        if dist > 10.0 then
          Just $ \_ -> pure $ a { action = Walking }
        else
          Nothing

    finish = \a -> Just \_ -> pure (a { action = Standing, goal = Nothing })
  in
    doFirst [ align, moveTowards, finish ] villager

-------------------- Helpers --------------------
rotate :: Number -> Vector2 -> Vector2
rotate deg v =
  let
    x = getX v

    y = getY v

    rad = Math.radians deg
  in
    (x * Math.cos rad - y * Math.sin rad) <=> (x * Math.sin rad + y * Math.cos rad)

angle :: Vector2 -> Vector2 -> Number
angle v1 v2 = Math.degrees $ Math.atan2 (determinant v1 v2) (dotProduct v1 v2)

doFirst :: forall a. Array (a -> Maybe (Unit -> Effect a)) -> a -> Effect a
doFirst choices e = do
  let
    f = choices # Array.find (\x -> Maybe.isJust $ x e) # unsafePartial fromJust

    g = f e # unsafePartial fromJust
  g unit

pick :: forall a. Array (Tuple Int (a -> Effect a)) -> a -> Effect a
pick choices e = do
  let
    sumChoices = map fst choices # sum

    aggregatedChoices =
      Array.foldl
        (\(Tuple acc total) (Tuple odds elem) -> Tuple (Tuple (total + odds) elem : acc) (total + odds))
        (Tuple [] 0)
        choices
  choice <- R.randomInt 1 sumChoices
  let
    f =
      aggregatedChoices
        # fst
        # Array.reverse
        # Array.filter (\(Tuple oddSum _) -> oddSum >= choice)
        # Array.head
        # map snd
        # unsafePartial fromJust
  f e
