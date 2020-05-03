module Apps.Village.Model where

import Model.Vector (Vector2)
import Data.Maybe (Maybe)

type World
  = { size :: Vector2, villager :: Villager }

type Villager
  = { pos :: Vector2, heading :: Vector2, action :: VillagerAction, goal :: Maybe Vector2 }

data VillagerAction
  = Standing
  | Walking
  | Turning Boolean
