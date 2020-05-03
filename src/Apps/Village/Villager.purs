module Apps.Village.Villager (render, tick, update) where

import Prelude
import Apps.Village.Helpers (doFirst, pick)
import Apps.Village.Model (Villager, VillagerAction(..), World)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random as R
import Graphics as G
import Model.Vector (Vector2, (|*|), rotate, getX, getY, angle, magnitude, (<=>))
import Toolkit (CanvasRuntime)

render :: Villager -> CanvasRuntime Unit
render villager = do
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

tick :: Villager -> Effect Villager
tick villager =
  pure
    $ case villager.action of
        Standing -> villager
        Walking -> villager { pos = villager.pos + villager.heading }
        Turning left -> villager { heading = rotate (if left then -1.0 else 1.0) villager.heading }

update :: World -> Villager -> Effect Villager
update world villager = case villager.goal of
  Nothing -> idle world villager
  Just g -> pursueGoal g villager

idle :: World -> Villager -> Effect Villager
idle world villager =
  let
    pickGoal = do
      x <- R.randomRange 0.0 (getX world.size)
      y <- R.randomRange 0.0 (getY world.size)
      pure (villager { goal = Just $ x <=> y })
  in
    pick
      [ Tuple 1 pickGoal
      , Tuple 9 (pure villager)
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
          Just $ pure $ a { action = Turning true }
        else
          if ang > 10.0 then
            Just $ pure $ a { action = Turning false }
          else
            Nothing

    moveTowards = \a ->
      let
        dist = magnitude (g - a.pos)
      in
        if dist > 10.0 then
          Just $ pure $ a { action = Walking }
        else
          Nothing

    finish = \a -> Just $ pure (a { action = Standing, goal = Nothing })
  in
    doFirst [ align, moveTowards, finish ] villager
