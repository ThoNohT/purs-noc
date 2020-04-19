-- | Stuff with vectors.
module Model.Vector where

import Prelude
import Effect (Effect)
import Effect.Random as Random
import Math as Math

-- | Defines all available operations on vectors.
class
  (Ring v) <= Vector v where
  -- | Scales the vector.
  scale :: Number -> v -> v
  -- | Returns the magnitude of the vector.
  magnitude :: v -> Number
  -- | Return the dot product of the vector.
  dotProduct :: v -> v -> Number
  -- | Creates a random vector.
  randomVector :: Effect v
  -- | Scales a vector to the specified magnitude.
  setMagnitude :: Number -> v -> v

-- | Normalizes a vector to a unit vector
normalize :: forall v. Vector v => v -> v
normalize = setMagnitude 1.0

-- | Flipped version of scale.
scaleFlipped :: forall v. Vector v => v -> Number -> v
scaleFlipped = flip scale

infixl 8 dotProduct as <.>

-- | A 2-dimensional vector, with an x, and a y component.
data Vector2
  = Vector2 Number Number

infix 7 Vector2 as <=>

instance semiringVector2 :: Semiring Vector2 where
  add (a <=> b) (c <=> d) = (a + c) <=> (b + d)
  zero = 0.0 <=> 0.0
  one = 1.0 <=> 1.0
  mul (a <=> b) (c <=> d) = (a * c) <=> (b * d) -- The Hamadard product.

instance ringVector2 :: Ring Vector2 where
  sub (a <=> b) (c <=> d) = (a - c) <=> (b - d)

instance vectorVector2 :: Vector Vector2 where
  scale s (x <=> y) = (x * s) <=> (y * s)
  dotProduct (a <=> b) (c <=> d) = (a * c) + (c * d)
  magnitude (x <=> y) = Math.sqrt (Math.pow x 2.0 + Math.pow y 2.0)
  randomVector = do
    x <- Random.randomRange (-1.0) 1.0
    y <- Random.randomRange (-1.0) 1.0
    pure $ setMagnitude 1.0 (x <=> y)
  setMagnitude mag v = scale (mag / magnitude v) v

infixl 8 scaleFlipped as |*|

-- | Get the x-component from a vector.
getX :: Vector2 -> Number
getX (x <=> _) = x

-- | Get the y-component from a vector.
getY :: Vector2 -> Number
getY (_ <=> y) = y

-- | Creates a 2-vector with the x and y components of the same value.
diagonal :: Number -> Vector2
diagonal dist = one |*| dist
