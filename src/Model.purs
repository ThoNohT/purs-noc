module Model where

type Interval
  = { milliseconds :: Number }

data KeyEvent
  = KeyUp
  | KeyDown

type KeyData
  = { event :: KeyEvent
    , keyCode :: String
    }

data MouseEvent
  = MouseMove
  | MouseDown
  | MouseUp

data MouseButton
  = LeftButton
  | RightButton
  | None

type Point
  = { x :: Number, y :: Number }

type MouseData
  = { event :: MouseEvent
    , button :: MouseButton
    , location :: Point
    }
