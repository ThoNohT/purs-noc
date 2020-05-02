module Apps.Village.Helpers where

import Prelude
import Data.Array ((:))
import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, sum)
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random as R
import Partial.Unsafe (unsafePartial)

-- | Pick the first function that returns some, and execute it.
doFirst :: forall f a. Foldable f => f (a -> Maybe (Effect a)) -> a -> Effect a
doFirst choices e = choices # foldMap (First <<< (#) e) # unwrap # fromMaybe (pure e)

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
