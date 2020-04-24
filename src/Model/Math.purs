module Model.Math (constrain) where

import Prelude
import Math as Math

-- | Constrains a number between two other numbers.
constrain :: Number -> Number -> Number -> Number
constrain min max number =
  number
    # Math.min max
    # Math.max min
