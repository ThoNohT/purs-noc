module Apps.Village.Helpers where

import Prelude
import Data.Array (zipWith, (:))
import Data.Foldable (class Foldable, foldMap, foldr, sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Random as R

-- | Pick the first function that returns some, and execute it.
doFirst :: forall f a. Foldable f => f (a -> Maybe (Effect a)) -> a -> Effect a
doFirst choices e = choices # foldMap (First <<< (#) e) # unwrap # fromMaybe (pure e)

-- | Pick a random function from the provided list and executed.
-- | Functions are tupled with a likelyhood for them to be picked.
pick :: forall a. Array (Tuple Int (Effect a)) -> a -> Effect a
pick choices e = do
  let
    sumChoices = map fst choices # sum
  choice <- R.randomInt 1 sumChoices
  let
    effects = map snd choices

    odds = map fst choices # foldr (\odd acc -> ((firstOrZero acc) + odd) : acc) []

    combine odd eff = \_ -> if odd <= choice then Just eff else Nothing

    aggregatedChoices = zipWith combine odds effects
  doFirst aggregatedChoices e

-- | Get the first element from a Foldable type, or zero if it is empty.
firstOrZero :: forall f a. Foldable f => Semiring a => f a -> a
firstOrZero = foldMap (Just >>> First) >>> unwrap >>> fromMaybe zero
