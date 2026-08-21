{- HLINT ignore "Eta reduce" -}
module Rapids.Rotate where

import Control.Lens
import Data.Fixed (mod')
import Linear hiding (rotate)
import Rapids.Color
import qualified Waterfall as W


-- | Rotate a 'Transformable' by radians around an axis specified in one of these ways:
class Rotate a where
  -- | @rotate@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > rotate x y z rad
  -- > rotate v     rad
  -- > rotate q
  -- > rotate ex    rad
  rotate :: a

instance {-# INCOHERENT #-} (x ~ Double, y ~ Double, z ~ Double, ang ~ Double, PropagateColor a, a' ~ a) => Rotate (x -> y -> z -> ang -> a -> a') where
  rotate x y z ang a = propagateColor (W.rotate (V3 x y z) (mod2pi ang)) a

mod2pi :: Double -> Double
mod2pi a = a `mod'` (2 * pi)

instance {-# OVERLAPPABLE #-} (d ~ Double, ang ~ Double, PropagateColor a, a' ~ a) => Rotate (V3 d -> ang -> a -> a') where
  rotate v ang a = propagateColor (W.rotate v (mod2pi ang)) a

instance {-# OVERLAPPABLE #-} (d ~ Double, PropagateColor a, a' ~ a) => Rotate (Quaternion d -> a -> a') where
  rotate q a = propagateColor (W.rotate (q ^. _yzw) (acos (q ^. _x))) a

instance {-# OVERLAPPABLE #-} (v ~ V3, ang ~ Double, PropagateColor a, a' ~ a) => Rotate (E v -> ang -> a -> a') where
  rotate (E e) ang a = propagateColor (W.rotate (0 & e .~ 1) (mod2pi ang)) a

class Rotated a where
  _rotated :: a

instance {-# INCOHERENT #-} (Profunctor p, Functor g, x ~ Double, y ~ Double, z ~ Double, ang ~ Double, PropagateColor a, a' ~ a) => Rotated (x -> y -> z -> ang -> Optic' p g a a') where
  _rotated x y z ang = iso (rotate x y z ang :: a' -> a) (rotate x y z (-ang) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, d ~ Double, ang ~ Double, PropagateColor a, a' ~ a) => Rotated (V3 d -> ang -> Optic' p g a a') where
  _rotated v ang = iso (rotate v ang :: a' -> a) (rotate v (-ang) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, d ~ Double, PropagateColor a, a' ~ a) => Rotated (Quaternion d -> Optic' p g a a') where
  _rotated q = iso (rotate q :: a' -> a) (propagateColor (W.rotate (negate (q ^. _yzw)) (acos (q ^. _x))) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, v ~ V3, ang ~ Double, PropagateColor a, a' ~ a) => Rotated (E v -> ang -> Optic' p g a a') where
  _rotated (E e) ang = iso (rotate (E e) ang :: a' -> a) (rotate (E e) (-ang) :: a -> a')


fromDeg :: Double -> Double
fromDeg a = mod2pi (a * pi / 180)

-- \| @rotateDeg@ expressions of type 'Transformable' @a => Iso' a a@ (probably Iso 'Solid' 'Solid')
--
-- > _rotatedDeg x y z deg
-- > _rotatedDeg v3 deg
-- > _rotatedDeg q  deg -- ignore the quaternion's magnitude
-- > _rotatedDeg ey deg
class RotateDeg r where
  rotateDeg :: r

instance {-# OVERLAPPABLE #-} (RotateDegGo r t, r ~ (t -> t)) => RotateDeg r where
  rotateDeg = rotateDegGo (id :: t -> t)

class W.Transformable t => RotateDegGo r t where
  rotateDegGo ::  (t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (PropagateColor a, a' ~ a, a~ t) => RotateDegGo (a -> a') t where
  rotateDegGo acc = propagateColor acc

instance {-# INCOHERENT #-} (deg ~ Double, x ~ Double, y ~ Double, z ~ Double, RotateDegGo r t) => RotateDegGo (x -> y -> z -> deg -> r) t where
  rotateDegGo acc x y z d = rotateDegGo (acc . W.rotate (V3 x y z) (fromDeg d))

instance {-# OVERLAPPABLE #-} (deg ~ Double, d ~ Double, RotateDegGo r t) => RotateDegGo (V3 d -> deg -> r) t where
  rotateDegGo acc v d = rotateDegGo (acc . W.rotate v (fromDeg d))

instance {-# OVERLAPPABLE #-} (deg ~ Double, d ~ Double, RotateDegGo r t) => RotateDegGo (Quaternion d -> deg -> r) t where
  rotateDegGo acc q d = rotateDegGo (acc . W.rotate (q ^. _yzw) (fromDeg d))

instance {-# OVERLAPPABLE #-} (deg ~ Double, v ~ V3, RotateDegGo r t) => RotateDegGo (E v -> deg -> r) t where
  rotateDegGo acc (E e) d = rotateDegGo (acc . W.rotate (0 & e .~ 1) (fromDeg d))

