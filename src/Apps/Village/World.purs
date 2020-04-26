module Apps.Village.World (World) where

import Model.Vector (Vector2)

-- | Represents the world the village lives in.
type World
  = { size :: Vector2 }
