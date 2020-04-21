-- | Stuff with vectors.
module Model.Vector where

import Prelude
import Math as Math
import Model.Random as Random
import Types (GenericRuntime)

-- | Defines all available operations on vectors.
class
  (CommutativeRing v, Show v) <= Vector v where
  -- | Scales the vector.
  scale :: Number -> v -> v
  -- | Returns the magnitude of the vector.
  magnitude :: v -> Number
  -- | Return the dot product of the vector.
  dotProduct :: v -> v -> Number
  -- | Creates a random vector.
  randomVector :: GenericRuntime v
  -- | Scales a vector to the specified magnitude.
  setMagnitude :: Number -> v -> v

-- | Normalizes a vector to a unit vector
normalize :: forall v. Vector v => v -> v
normalize = setMagnitude 1.0

-- | Flipped version of scale.
scaleFlipped :: forall v. Vector v => v -> Number -> v
scaleFlipped = flip scale

infixl 8 scaleFlipped as |*|

-- | Inverted version of scaleFlipped.
invScaleFlipped :: forall v. Vector v => v -> Number -> v
invScaleFlipped v scale = scaleFlipped v (1.0 / scale)

infixl 8 invScaleFlipped as |/|

-- | Limits the magnitude of a vector.
limit :: forall v. Vector v => Number -> v -> v
limit maxMag v = if magnitude v > maxMag then setMagnitude maxMag v else v

infixl 8 dotProduct as <.>

-- | A 2-dimensional vector, with an x, and a y component.
data Vector2
  = Vector2 Number Number

infix 7 Vector2 as <=>

instance showVector2 :: Show Vector2 where
  show v = show (getX v) <> " <=> " <> show (getY v)

instance semiringVector2 :: Semiring Vector2 where
  add (a <=> b) (c <=> d) = (a + c) <=> (b + d)
  zero = 0.0 <=> 0.0
  one = 1.0 <=> 1.0
  mul (a <=> b) (c <=> d) = (a * c) <=> (b * d) -- The Hamadard product.

instance ringVector2 :: Ring Vector2 where
  sub (a <=> b) (c <=> d) = (a - c) <=> (b - d)

instance commutativeRingVector2 :: CommutativeRing Vector2

instance vectorVector2 :: Vector Vector2 where
  scale s (x <=> y) = (x * s) <=> (y * s)
  dotProduct (a <=> b) (c <=> d) = (a * c) + (c * d)
  magnitude (x <=> y) = Math.sqrt (Math.pow x 2.0 + Math.pow y 2.0)
  randomVector = do
    x <- Random.randomRange (-1.0) 1.0
    y <- Random.randomRange (-1.0) 1.0
    pure $ setMagnitude 1.0 (x <=> y)
  setMagnitude mag v = if magnitude v == 0.0 then v else scale (mag / magnitude v) v

-- | Get the x-component from a vector.
getX :: Vector2 -> Number
getX (x <=> _) = x

-- | Get the y-component from a vector.
getY :: Vector2 -> Number
getY (_ <=> y) = y

-- | Set the x-component of a vector.
setX :: Number -> Vector2 -> Vector2
setX x (_ <=> y) = x <=> y

-- | Set the y-component of a vector.
setY :: Number -> Vector2 -> Vector2
setY y (x <=> _) = x <=> y

-- | Creates a 2-vector with the x and y components of the same value.
diagonal :: Number -> Vector2
diagonal dist = one |*| dist
