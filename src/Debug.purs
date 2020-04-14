module Debug (unsafeLog) where

foreign import unsafeLog :: forall a. a -> a
