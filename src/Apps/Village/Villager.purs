module Apps.Village.Villager (init, render, tick, update) where

import Prelude
import Apps.Village.Helpers (doFirst, pick, toHex)
import Apps.Village.Model (Villager, VillagerAction(..), VillagerActivity(..), World)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random as R
import Graphics as G
import Model.Vector (Vector2, (|*|), rotate, getX, getY, angle, magnitude, (<=>))
import Toolkit (CanvasRuntime)

init :: Vector2 -> Villager
init pos =
  { pos: pos
  , action: Standing
  , heading: (1.0 <=> 0.0)
  , goal: Nothing
  , energy: 1000.0
  , activity: Idle
  }

render :: Villager -> CanvasRuntime Unit
render villager = do
  let
    intEnergy = floor villager.energy / 4

    red = 255 - intEnergy # toHex

    green = intEnergy # toHex
  G.setFillStyle $ "#" <> red <> green <> "00"
  G.setStrokeStyle "white"
  G.setStrokeWidth 2.0
  G.circle villager.pos 10.0
  case villager.action of
    Sleeping -> pure unit
    _ -> G.line villager.pos (villager.pos + villager.heading |*| 20.0)
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
        Sleeping -> villager
        Walking -> villager { pos = villager.pos + (villager.heading |*| (0.5 + (villager.energy / 2000.0))) }
        Turning left -> villager { heading = rotate (if left then -1.0 else 1.0) villager.heading }

update :: World -> Villager -> Effect Villager
update world villager = villager # updateActivity world >>= handleGoal <#> consumeEnergy

updateActivity :: World -> Villager -> Effect Villager
updateActivity world villager = case villager.activity of
  Idle -> idle world villager
  _ -> pure villager -- Maybe add the ability to abort another activity if the villager is tired?

idle :: World -> Villager -> Effect Villager
idle world villager =
  let
    startWandering = do
      x <- R.randomRange 0.0 (getX world.size)
      y <- R.randomRange 0.0 (getY world.size)
      pure (villager { activity = Wandering, goal = Just $ x <=> y })

    startResting = pure (villager { activity = Resting, goal = Just world.bed.pos })
  in
    pick
      -- All odds depend on energy level.
      [ Tuple (if villager.energy < 250.0 then 1 else 0) startResting
      , Tuple (if villager.energy > 250.0 then 1 else 0) startWandering
      , Tuple (if villager.energy > 250.0 then 9 else 0) $ pure villager
      ]
      villager

handleGoal :: Villager -> Effect Villager
handleGoal villager = case villager.activity of
  Idle -> pure villager
  Wandering -> case villager.goal of
    Nothing -> pure $ villager { activity = Idle }
    Just g -> pursueGoal g villager
  Resting -> case villager.goal of
    Nothing ->
      if villager.energy > 999.0 then
        pure $ villager { activity = Idle, action = Standing }
      else
        pure villager { action = Sleeping }
    Just g -> pursueGoal g villager

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

consumeEnergy :: Villager -> Villager
consumeEnergy villager = villager { energy = max 0.0 (villager.energy - energyDrain) }
  where
  energyDrain = case villager.action of
    Walking -> 1.0 * (villager.energy / 1000.0)
    Turning _ -> 0.1
    Standing -> 0.01
    Sleeping -> -5.0
