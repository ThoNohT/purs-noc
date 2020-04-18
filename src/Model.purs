module Model where

-- | Defines an interval in milliseconds.
type Interval
  = { milliseconds :: Number }

-- | The possible types of keyboard events that can happen.
data KeyEvent
  = KeyUp
  | KeyDown

-- | The data in a keyboard event.
type KeyData
  = { event :: KeyEvent
    , keyCode :: String
    }

-- | The possible types of mouse events that can happen.
data MouseEvent
  = MouseMove
  | MouseDown
  | MouseUp

-- | The possible mouse buttons a mouse event can report about.
data MouseButton
  = LeftButton
  | RightButton
  | None

-- | A point, with an x, and a y component.
type Point
  = { x :: Number, y :: Number }

-- | The data in a mouse event.
type MouseData
  = { event :: MouseEvent
    , button :: MouseButton
    , location :: Point
    }
