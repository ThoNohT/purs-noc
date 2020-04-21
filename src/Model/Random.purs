module Model.Random where

import Prelude
import Effect.Random as R
import Types (GenericRuntime, liftEffect)

-- | Generates a random number in the provided range.
randomRange :: Number -> Number -> GenericRuntime Number
randomRange from to = liftEffect $ R.randomRange from to
