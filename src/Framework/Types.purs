module Framework.Types where

import Prelude
import Control.Monad.State (class MonadTrans, StateT, lift)
import Effect (Effect)

-- | A generic versioon of the CanvasRuntime.
type GenericRuntime r
  = forall a. StateT a Effect r

-- | Lifts effects into the generic runtime.
liftEffect :: forall m a t. MonadTrans t => Monad m => m a -> t m a
liftEffect = lift
