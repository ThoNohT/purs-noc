module Toolkit (CanvasApp, CanvasRuntime, defaultApp) where

import Prelude

import Control.Monad.State (StateT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Model.Events (KeyData, MouseData)

-- | The runtime type that is used when performing actions in an application, like rendering or updating the
-- | state.
type CanvasRuntime context =  StateT context Effect

-- | The specification of an app
type CanvasApp context state
  = { initialState :: state
    , tick :: state -> CanvasRuntime context (Maybe state)
    , handleKeyboard :: KeyData -> state -> CanvasRuntime context (Maybe state)
    , handleMouse :: MouseData -> state -> CanvasRuntime context (Maybe state)
    , initialize :: state -> CanvasRuntime context (Maybe state)
    , render :: state -> CanvasRuntime context Unit
    , updateInterval :: Int
    }

-- | A default canvas app, that does nothing. Override individual properties of this specification to add behaviour to
-- | an app without having to always define the entire application.
defaultApp :: forall context state. state -> CanvasApp context state
defaultApp initialState =
  { initialState: initialState
  , tick: const $ pure Nothing
  , handleKeyboard: const2 $ pure Nothing
  , handleMouse: const2 $ pure Nothing
  , render: const (pure unit)
  , initialize: const (pure Nothing)
  , updateInterval: 33
  }

-- Helpers --
const2 :: forall a b c. a -> b -> c -> a
const2 a _ _ = a
