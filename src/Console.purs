module Console (log, unsafeLog) where

import Prelude
import Effect.Class.Console as Console
import Types (GenericRuntime, liftEffect)

-- | Safe version of logging to the console that returns an effect.
log :: forall a. Show a => a -> GenericRuntime Unit
log x = liftEffect $ Console.logShow x

-- | Unsafe version of logging to the console that does not return an Effect.
foreign import unsafeLog :: forall a. a -> a
