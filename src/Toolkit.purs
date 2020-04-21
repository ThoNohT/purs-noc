module Toolkit (CanvasApp, defaultApp) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Model.Events (KeyData, MouseData)
import Graphics (GraphicsContext)

-- | The specification of an app
type CanvasApp state
  = { initialState :: state
    , tick :: state -> Effect (Maybe state)
    , handleKeyboard :: KeyData -> state -> Effect (Maybe state)
    , handleMouse :: MouseData -> state -> Effect (Maybe state)
    , initialize :: GraphicsContext -> state -> Effect (Maybe state)
    , render :: GraphicsContext -> state -> Effect Unit
    , updateInterval :: Int
    }

defaultApp :: forall state. state -> CanvasApp state
defaultApp initialState =
  { initialState: initialState
  , tick: const $ pure Nothing
  , handleKeyboard: const2 $ pure Nothing
  , handleMouse: const2 $ pure Nothing
  , render: const2 (pure unit)
  , initialize: const2 (pure Nothing)
  , updateInterval: 33
  }

-- Helpers --
const2 :: forall a b c. a -> b -> c -> a
const2 a _ _ = a
