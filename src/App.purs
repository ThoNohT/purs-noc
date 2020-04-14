module App (CanvasApp, app, defaultAppSpec) where

import Prelude
import Control.Monad.State as HS
import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Model (Interval, KeyData, MouseData)

type CanvasApp
  = H.Component HH.HTML (Const Void) Unit Void Aff

type CanvasAppSpec state
  = { initialState :: state
    , tick :: Interval -> state -> state
    , handleKeyboard :: KeyData -> state -> state
    , handleMouse :: MouseData -> state -> state
    , render :: state -> Effect Unit
    }

data Msg
  = Tick Interval
  | Keyboard KeyData
  | Mouse MouseData
  | Render

defaultAppSpec :: forall state. state -> CanvasAppSpec state
defaultAppSpec initialState =
  { initialState: initialState
  , tick: \_ s -> s
  , handleKeyboard: \_ s -> s
  , handleMouse: \_ s -> s
  , render: const (pure unit)
  }

view :: H.ComponentHTML Msg () Aff
view = HH.canvas [ HP.id_ "render-canvas", HP.width 800, HP.height 600 ]

update ::
  forall state.
  CanvasAppSpec state ->
  Msg ->
  H.HalogenM state Msg () Void Aff Unit
update appSpec = case _ of
  Tick interval -> HS.modify_ (appSpec.tick interval)
  Keyboard kbData -> HS.modify_ (appSpec.handleKeyboard kbData)
  Mouse mouseData -> HS.modify_ (appSpec.handleMouse mouseData)
  Render -> do
    currentState <- HS.get
    H.liftEffect (appSpec.render currentState)

app ::
  forall state.
  CanvasAppSpec state ->
  H.Component HH.HTML (Const Void) Unit Void Aff
app appSpec =
  H.mkComponent
    { initialState: const appSpec.initialState
    , render: const view
    , eval: H.mkEval $ H.defaultEval { handleAction = update appSpec }
    }
