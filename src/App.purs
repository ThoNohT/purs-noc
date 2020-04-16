module App (CanvasApp, app, defaultAppSpec) where

import Prelude
import Control.Monad.State as HS
import Data.Const (Const)
import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model (Interval, KeyData, KeyEvent(..), MouseButton(..), MouseData, MouseEvent(..))
import Partial.Unsafe (unsafePartial)
import Web.DOM.NonElementParentNode as NEPN
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME
import Halogen.Query.EventSource as ES
import Effect.Timer as Timer

-- App specification --
type CanvasApp
  = H.Component HH.HTML (Const Void) Unit Void Aff

type CanvasAppSpec state
  = { initialState :: state
    , tick :: Interval -> state -> Maybe state
    , handleKeyboard :: KeyData -> state -> Maybe state
    , handleMouse :: MouseData -> state -> Maybe state
    , render :: state -> Effect Unit
    , updateInterval :: Int
    }

data Action
  = Init
  | Tick Interval
  | Keyboard KeyData
  | Mouse MouseData
  | Render

defaultAppSpec :: forall state. state -> CanvasAppSpec state
defaultAppSpec initialState =
  { initialState: initialState
  , tick: const2 Nothing
  , handleKeyboard: const2 Nothing
  , handleMouse: const2 Nothing
  , render: const (pure unit)
  , updateInterval: 33
  }

-- Component implementation --
type ComponentState state
  = { changed :: Boolean, state :: state }

view :: H.ComponentHTML Action () Aff
view =
  HH.canvas
    [ HP.id_ "render-canvas"
    , HP.width 800
    , HP.height 600
    , HP.tabIndex 0
    , HE.onMouseMove (toMouseData MouseMove >>> Mouse >>> Just)
    , HE.onMouseDown (toMouseData MouseDown >>> Mouse >>> Just)
    , HE.onMouseUp (toMouseData MouseUp >>> Mouse >>> Just)
    , HE.onKeyDown (toKeyData KeyDown >>> Keyboard >>> Just)
    , HE.onKeyUp (toKeyData KeyUp >>> Keyboard >>> Just)
    ]

update ::
  forall state.
  CanvasAppSpec state ->
  Action ->
  H.HalogenM (ComponentState state) Action () Void Aff Unit
update appSpec = case _ of
  Init -> do
    let
      eventSource =
        ES.effectEventSource
          $ \emitter -> do
              let
                passTick = ES.emit emitter (Tick { milliseconds: appSpec.updateInterval })
              intervalId <- Timer.setInterval appSpec.updateInterval passTick
              pure $ ES.Finalizer (Timer.clearInterval intervalId)
    _ <- H.subscribe $ eventSource
    H.liftEffect $ focusElement "render-canvas"
  Tick interval -> mapState $ appSpec.tick interval
  Keyboard kbData -> mapState $ appSpec.handleKeyboard kbData
  Mouse mouseData -> mapState $ appSpec.handleMouse mouseData
  Render -> do
    currentState <- HS.get
    if currentState.changed then H.liftEffect (appSpec.render currentState.state) else H.liftEffect $ pure unit
    HS.put currentState { changed = false }

app ::
  forall state.
  CanvasAppSpec state ->
  H.Component HH.HTML (Const Void) Unit Void Aff
app appSpec =
  H.mkComponent
    { initialState: const { changed: true, state: appSpec.initialState }
    , render: const view
    , eval: H.mkEval $ H.defaultEval { handleAction = update appSpec, initialize = Just Init }
    }

-- Helpers --
const2 :: forall a b c. a -> b -> c -> a
const2 a _ _ = a

toKeyData :: KeyEvent -> KE.KeyboardEvent -> KeyData
toKeyData eventType event =
  { event: eventType
  , keyCode: KE.code event
  }

toMouseData :: MouseEvent -> ME.MouseEvent -> MouseData
toMouseData eventType event =
  let
    buttonInt = ME.buttons event

    button =
      if buttonInt .&. 1 > 0 then
        LeftButton
      else
        if buttonInt .&. 2 > 0 then
          RightButton
        else
          None
  in
    { event: eventType
    , button: button
    , location: { x: ME.clientX event, y: ME.clientY event }
    }

mapState :: forall state. (state -> Maybe state) -> H.HalogenM (ComponentState state) Action () Void Aff Unit
mapState f = do
  currentState <- HS.get
  let
    newState = f currentState.state
  case newState of
    Just s -> HS.put currentState { changed = true, state = s }
    _ -> H.liftEffect $ pure unit

focusElement :: String -> Effect Unit
focusElement elementId = do
  document <- H.liftEffect $ Web.document =<< Web.window
  element <- H.liftEffect $ NEPN.getElementById elementId $ HTMLDocument.toNonElementParentNode document
  let
    element' = unsafePartial fromJust (element >>= HTMLElement.fromElement)
  HTMLElement.focus element'
