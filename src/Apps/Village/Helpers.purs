module Apps.Village.Helpers where

import Prelude
import Data.Array as Array
import Data.Array ((:))
import Data.Foldable (sum)
import Data.Maybe (Maybe, fromJust, isJust)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random as R
import Partial.Unsafe (unsafePartial)

-- | Pick the first function that returns some, and execute it.
doFirst :: forall a. Array (a -> Maybe (Unit -> Effect a)) -> a -> Effect a
doFirst choices e = do
  let
    f = choices # Array.find (\x -> isJust $ x e) # unsafePartial fromJust

    g = f e # unsafePartial fromJust
  g unit

-- | Pick a random function from the provided list and executed.
-- | Functions are tupled with a likelyhood for them to be picked.
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
