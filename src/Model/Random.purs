module Model.Random where

import Prelude
import Effect.Random as R
import Framework.Types (GenericRuntime, liftEffect)

-- | Generates a random number in the provided range.
randomRange :: Number -> Number -> GenericRuntime Number
randomRange from to = liftEffect $ R.randomRange from to

-- | Generates a random int between the provided numbers.
randomInt :: Int -> Int -> GenericRuntime Int
randomInt low high = liftEffect $ R.randomInt low high
