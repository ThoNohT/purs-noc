module Apps.Village.World (render, tick, update) where

import Prelude
import Apps.Village.Model (World)
import Apps.Village.Villager as Villager
import Effect (Effect)
import Graphics as G
import Toolkit (CanvasRuntime)

render :: World -> CanvasRuntime Unit
render world = do
  G.background "green"
  Villager.render world.villager

tick :: World -> Effect World
tick world = do
  newVillager <- Villager.tick world.villager
  pure $ world { villager = newVillager }

update :: World -> Effect World
update world = do
  newVillager <- Villager.update world world.villager
  pure $ world { villager = newVillager }
