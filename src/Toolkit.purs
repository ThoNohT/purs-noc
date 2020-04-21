module Toolkit (CanvasApp, CanvasRuntime, defaultApp) where

import Prelude

import Control.Monad.State (StateT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics (GraphicsContext)
import Model.Events (KeyData, MouseData)

-- | The runtime type that is used when performing actions in an application, like rendering or updating the
-- | state.
type CanvasRuntime = StateT GraphicsContext Effect

-- | The specification of an app
type CanvasApp state
  = { initialState :: state
    , tick :: state -> CanvasRuntime (Maybe state)
    , handleKeyboard :: KeyData -> state -> CanvasRuntime (Maybe state)
    , handleMouse :: MouseData -> state -> CanvasRuntime (Maybe state)
    , initialize :: state -> CanvasRuntime (Maybe state)
    , render :: state -> CanvasRuntime Unit
    , updateInterval :: Int
    }

-- | A default canvas app, that does nothing. Override individual properties of this specification to add behaviour to
-- | an app without having to always define the entire application.
defaultApp :: forall state. state -> CanvasApp state
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
