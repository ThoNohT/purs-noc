module App (create) where

import Prelude
import Control.Monad.State as HS
import Data.Const (Const)
import Data.Int (toNumber)
import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Timer as Timer
import Graphics (GraphicsContext, makeContextForElement)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Model.Events (KeyData, KeyEvent(..), MouseButton(..), MouseData, MouseEvent(..))
import Model.Vector ((<=>))
import Types (CanvasApp)
import Partial.Unsafe (unsafePartial)
import Web.DOM.NonElementParentNode as NEPN
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document) as Web
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

-- App specification --
data Action
  = Init
  | Tick
  | Keyboard KeyData
  | Mouse MouseData
  | Render (ES.Emitter Effect Action)

-- Component implementation --
type ComponentState state
  = { changed :: Boolean, state :: state, context :: Maybe GraphicsContext }

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
  CanvasApp state ->
  Action ->
  H.HalogenM (ComponentState state) Action () Void Aff Unit
update appSpec = case _ of
  Init -> do
    _ <- H.subscribe $ tickSource appSpec.updateInterval
    _ <- H.subscribe $ renderSource
    H.liftEffect $ focusElement "render-canvas"
    ctx <- H.liftEffect $ makeContextForElement "render-canvas"
    HS.modify_ $ \s -> s { context = Just ctx }
    mapState $ appSpec.initialize ctx
  Tick -> mapState $ appSpec.tick
  Keyboard kbData -> mapState $ appSpec.handleKeyboard kbData
  Mouse mouseData -> mapState $ appSpec.handleMouse mouseData
  Render emitter -> do
    currentState <- HS.get
    case { changed: currentState.changed, context: currentState.context } of
      { changed: true, context: Just ctx } -> H.liftEffect (appSpec.render ctx currentState.state)
      _ -> H.liftEffect $ pure unit
    HS.put currentState { changed = false }
    H.liftEffect
      $ do
          let
            passRender = ES.emit emitter (Render emitter)
          window <- Web.window
          _ <- H.liftEffect $ Window.requestAnimationFrame passRender window
          pure unit

create :: forall state. CanvasApp state -> H.Component HH.HTML (Const Void) Unit Void Aff
create appSpec = do
  H.mkComponent
    { initialState: const { changed: false, state: appSpec.initialState, context: Nothing }
    , render: const view
    , eval: H.mkEval $ H.defaultEval { handleAction = update appSpec, initialize = Just Init }
    }

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
    , location: (ME.clientX event # toNumber) <=> (ME.clientY event # toNumber)
    }

mapState :: forall state. (state -> Effect (Maybe state)) -> H.HalogenM (ComponentState state) Action () Void Aff Unit
mapState f = do
  currentState <- HS.get
  newState <- H.liftEffect $ f currentState.state
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

tickSource :: forall a. MonadAff a => Int -> ES.EventSource a Action
tickSource interval =
  ES.effectEventSource
    $ \emitter -> do
        let
          passTick = ES.emit emitter Tick
        intervalId <- Timer.setInterval interval passTick
        pure $ ES.Finalizer (Timer.clearInterval intervalId)

renderSource :: forall a. MonadAff a => ES.EventSource a Action
renderSource =
  ES.effectEventSource
    $ \emitter -> do
        let
          passRender = ES.emit emitter (Render emitter)
        window <- Web.window
        frameId <- Window.requestAnimationFrame passRender window
        pure $ ES.Finalizer (Window.cancelAnimationFrame frameId window)
