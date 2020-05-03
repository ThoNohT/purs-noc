module Apps.Village.World (init, render, tick, update) where

import Prelude
import Apps.Village.Bed as Bed
import Apps.Village.Model (World)
import Apps.Village.Villager as Villager
import Effect (Effect)
import Graphics as G
import Model.Vector (Vector2, (<=>))
import Toolkit (CanvasRuntime)

init :: Vector2 -> World
init size =
  { size: size
  , villager: Villager.init (100.0 <=> 100.0)
  , bed: Bed.init (500.0 <=> 200.0)
  }

render :: World -> CanvasRuntime Unit
render world = do
  G.background "green"
  Bed.render world.bed
  Villager.render world.villager

tick :: World -> Effect World
tick world = do
  newVillager <- Villager.tick world.villager
  newBed <- Bed.tick world.bed
  pure $ world { villager = newVillager, bed = newBed }

update :: World -> Effect World
update world = do
  newVillager <- Villager.update world world.villager
  newBed <- Bed.update world.bed
  pure $ world { villager = newVillager, bed = newBed }
