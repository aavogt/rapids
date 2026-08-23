module Rapids.Rotate where

import Control.Lens
import Data.Fixed (mod')
import Linear hiding (rotate)
import Rapids.Color
import qualified Waterfall as W

mod2pi :: Double -> Double
mod2pi a = a `mod'` (2 * pi)


-- * interface

-- | Rotate a 'Transformable' by radians around an axis specified in one of these ways:
--
-- > rotate x y z rad
-- > rotate v     rad
-- > rotate q
-- > rotate ex    rad
rotate :: (RotateGo r t) => r
rotate = rotateGo (id :: t -> t)

class W.Transformable t => RotateGo r t | r -> t where
  rotateGo :: (t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (PropagateColor a, a' ~ a, a ~ t) => RotateGo (a -> a') t where
  rotateGo acc = propagateColor acc

instance {-# INCOHERENT #-} (ang ~ Double, x ~ Double, y ~ Double, z ~ Double, RotateGo (r -> s) t) => RotateGo (x -> y -> z -> ang -> r -> s) t where
  rotateGo acc x y z ang = rotateGo (acc . W.rotate (V3 x y z) (mod2pi ang))

instance {-# OVERLAPPABLE #-} (ang ~ Double, d ~ Double, RotateGo r t) => RotateGo (V3 d -> ang -> r) t where
  rotateGo acc v ang = rotateGo (acc . W.rotate v (mod2pi ang))

instance {-# OVERLAPPABLE #-} (d ~ Double, RotateGo (r -> s) t) => RotateGo (Quaternion d -> r -> s) t where
  rotateGo acc q = rotateGo (acc . W.rotate (q ^. _yzw) (acos (q ^. _x)))

instance {-# OVERLAPPABLE #-} (v ~ V3, ang ~ Double, RotateGo r t) => RotateGo (E v -> ang -> r) t where
  rotateGo acc (E e) ang = rotateGo (acc . W.rotate (0 & e .~ 1) (mod2pi ang))

-- | Rotate through an 'Iso'.
_rotated :: (RotatedGo r t) => r
_rotated = rotatedGo id id


class W.Transformable t => RotatedGo r t | r -> t where
  rotatedGo :: (t -> t) -> (t -> t) -> r
instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, PropagateColor a, a' ~ a, a ~ t) => RotatedGo (Optic' p g a a') t where
  rotatedGo forward backward = iso (propagateColor forward :: a' -> a) (propagateColor backward :: a -> a')

instance {-# INCOHERENT #-} (ang ~ Double, x ~ Double, y ~ Double, z ~ Double, RotatedGo r t) => RotatedGo (x -> y -> z -> ang -> r) t where
  rotatedGo forward backward x y z ang =
    let rotation = W.rotate (V3 x y z) (mod2pi ang)
        inverse = W.rotate (V3 x y z) (mod2pi (-ang))
     in rotatedGo (forward . rotation) (inverse . backward)

instance {-# OVERLAPPABLE #-} (ang ~ Double, d ~ Double, RotatedGo r t) => RotatedGo (V3 d -> ang -> r) t where
  rotatedGo forward backward v ang =
    let rotation = W.rotate v (mod2pi ang)
        inverse = W.rotate v (mod2pi (-ang))
     in rotatedGo (forward . rotation) (inverse . backward)

instance {-# OVERLAPPABLE #-} (d ~ Double, RotatedGo r t) => RotatedGo (Quaternion d -> r) t where
  rotatedGo forward backward q =
    let angle = acos (q ^. _x)
        rotation = W.rotate (q ^. _yzw) angle
        inverse = W.rotate (q ^. _yzw) (-angle)
     in rotatedGo (forward . rotation) (inverse . backward)

instance {-# OVERLAPPABLE #-} (v ~ V3, ang ~ Double, RotatedGo r t) => RotatedGo (E v -> ang -> r) t where
  rotatedGo forward backward (E e) ang =
    let rotation = W.rotate (0 & e .~ 1) (mod2pi ang)
        inverse = W.rotate (0 & e .~ 1) (mod2pi (-ang))
     in rotatedGo (forward . rotation) (inverse . backward)

fromDeg :: Double -> Double
fromDeg a = mod2pi (a * pi / 180)

-- \| @rotateDeg@ expressions of type 'Transformable' @a => Iso' a a@ (probably Iso 'Solid' 'Solid')
--
-- > _rotatedDeg x y z deg
-- > _rotatedDeg v3 deg
-- > _rotatedDeg q  deg -- ignore the quaternion's magnitude
-- > _rotatedDeg ey deg
-- | Rotate by degrees around an axis specified in one of these ways.
rotateDeg :: (RotateDegGo r t) => r
rotateDeg = rotateDegGo (id :: t -> t)

class W.Transformable t => RotateDegGo r t | r -> t where
  rotateDegGo ::  (t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (PropagateColor a, a' ~ a, a~ t) => RotateDegGo (a -> a') t where
  rotateDegGo acc = propagateColor acc

instance {-# INCOHERENT #-} (deg ~ Double, x ~ Double, y ~ Double, z ~ Double, RotateDegGo (r -> s) t) => RotateDegGo (x -> y -> z -> deg -> r -> s) t where
  rotateDegGo acc x y z d = rotateDegGo (acc . W.rotate (V3 x y z) (fromDeg d))

instance {-# OVERLAPPABLE #-} (deg ~ Double, d ~ Double, RotateDegGo r t) => RotateDegGo (V3 d -> deg -> r) t where
  rotateDegGo acc v d = rotateDegGo (acc . W.rotate v (fromDeg d))

instance {-# OVERLAPPABLE #-} (deg ~ Double, d ~ Double, RotateDegGo r t) => RotateDegGo (Quaternion d -> deg -> r) t where
  rotateDegGo acc q d = rotateDegGo (acc . W.rotate (q ^. _yzw) (fromDeg d))

instance {-# OVERLAPPABLE #-} (deg ~ Double, v ~ V3, RotateDegGo r t) => RotateDegGo (E v -> deg -> r) t where
  rotateDegGo acc (E e) d = rotateDegGo (acc . W.rotate (0 & e .~ 1) (fromDeg d))
