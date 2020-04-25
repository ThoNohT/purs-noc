module Model.Math (constrain, degrees, radians) where

import Prelude
import Math as Math

-- | Constrains a number between two other numbers.
constrain :: Number -> Number -> Number -> Number
constrain min max number =
  number
    # Math.min max
    # Math.max min

-- | Converts degrees into radians.
radians :: Number -> Number
radians deg = (deg / 360.0) * Math.pi * 2.0

-- | Converts radians into degrees.
degrees :: Number -> Number
degrees rad = 360.0 * rad / (Math.pi * 2.0)
