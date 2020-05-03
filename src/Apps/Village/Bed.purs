module Apps.Village.Bed (init, render, tick, update) where

import Prelude
import Apps.Village.Model (Bed)
import Effect (Effect)
import Graphics as G
import Model.Vector (Vector2)
import Toolkit (CanvasRuntime)

init :: Vector2 -> Bed
init pos = { pos: pos }

render :: Bed -> CanvasRuntime Unit
render bed = do
  G.setFillStyle "blue"
  G.setStrokeStyle "white"
  G.setStrokeWidth 1.0
  G.circle bed.pos 3.0

tick :: Bed -> Effect Bed
tick bed = pure bed

update :: Bed -> Effect Bed
update bed = do pure bed
