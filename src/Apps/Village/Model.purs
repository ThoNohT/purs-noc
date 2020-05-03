module Apps.Village.Model where

import Model.Vector (Vector2)
import Data.Maybe (Maybe)

type World
  = { size :: Vector2, villager :: Villager, bed :: Bed }

type Villager
  = { pos :: Vector2
    , heading :: Vector2
    , action :: VillagerAction
    , activity :: VillagerActivity
    , goal :: Maybe Vector2
    , energy :: Number
    }

type Bed
  = { pos :: Vector2 }

data VillagerAction
  = Standing
  | Walking
  | Turning Boolean
  | Sleeping

data VillagerActivity
  = Idle
  | Wandering
  | Resting
