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
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Model (Interval, KeyData, KeyEvent(..), MouseButton(..), MouseData, MouseEvent(..))
import Partial.Unsafe (unsafePartial)
import Web.DOM.Element as E
import Web.DOM.NonElementParentNode as NEPN
import Web.HTML (HTMLDocument)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as MET

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
  = Init
  | Tick Interval
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

update ::
  forall state.
  CanvasAppSpec state ->
  Msg ->
  H.HalogenM state Msg () Void Aff Unit
update appSpec = case _ of
  Init -> do
    document <- H.liftEffect $ Web.document =<< Web.window
    element <- H.liftEffect $ NEPN.getElementById "render-canvas" $ HTMLDocument.toNonElementParentNode document
    let
      element' = unsafePartial fromJust element
    H.subscribe' \_ ->
      ES.eventListenerEventSource
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (toKeyData KeyUp >>> Keyboard) <<< KE.fromEvent)
    H.subscribe' \_ ->
      ES.eventListenerEventSource
        KET.keydown
        (HTMLDocument.toEventTarget document)
        (map (toKeyData KeyDown >>> Keyboard) <<< KE.fromEvent)
    H.subscribe' \_ ->
      ES.eventListenerEventSource
        MET.mousemove
        (E.toEventTarget element')
        (map (toMouseData MouseMove >>> Mouse) <<< ME.fromEvent)
    H.subscribe' \_ ->
      ES.eventListenerEventSource
        MET.mousedown
        (E.toEventTarget element')
        (map (toMouseData MouseDown >>> Mouse) <<< ME.fromEvent)
    H.subscribe' \_ ->
      ES.eventListenerEventSource
        MET.mouseup
        (E.toEventTarget element')
        (map (toMouseData MouseUp >>> Mouse) <<< ME.fromEvent)
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
    , eval: H.mkEval $ H.defaultEval { handleAction = update appSpec, initialize = Just Init }
    }
