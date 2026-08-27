module Rapids.Rotate where

import Control.Lens
import Data.Fixed (mod')
import Linear hiding (rotate)
import Rapids.Color
import qualified Waterfall as W

-- * interface

-- | Rotate a 'Transformable' by radians around an axis specified in one of these ways:
--
-- > rotate x y z rad :: Transformable a => a -> a
-- > rotate v     rad
-- > rotate q
-- > rotate ex    rad
rotate :: (RotateGo r t) => r
rotate = rotateGo id \axis angle x -> W.rotate axis (mod2pi angle) x

rotated :: (Num t, RotateGo r t) => r
rotated = rotateGo id \axis angle x -> x + W.rotate axis (mod2pi angle) x

-- \ Rotate a 'Transformable' by degrees around an axis specified in one of these ways:
--
-- > _rotatedDeg x y z deg
-- > _rotatedDeg v3 deg
-- > _rotatedDeg q  deg -- ignore the quaternion's magnitude
-- > _rotatedDeg ey deg
rotateDeg :: (RotateGo r t) => r
rotateDeg = rotateGo id \axis angle x -> W.rotate axis (fromDeg angle) x

-- | 'rotatedDeg' is 'rotateDeg' which also adds the original at each step
rotatedDeg :: (Num t, RotateGo r t) => r
rotatedDeg = rotateGo id \axis angle x -> x + W.rotate axis (fromDeg angle) x

-- | @_rotated@ produces a type-changing 'Iso' using the same arguments as 'rotate'.
-- The input is rotated before the operation; its result is rotated back.
_rotated :: (Rotated'Go r t) => r
_rotated = rotated'Go (Transform3D id) (Transform3D id)

-- | @_rotated'@ is the endomorphic, overloaded form of '_rotated'.
_rotated' :: (RotatedGo r t) => r
_rotated' = rotatedGo id id

-- * implementation

fromDeg :: Double -> Double
fromDeg a = (a * pi / 180) `mod'` (2 * pi)

mod2pi :: Double -> Double
mod2pi a = a `mod'` (2 * pi)

class (W.Transformable t) => Rotated'Go r t | r -> t where
  rotated'Go :: Transform3D -> Transform3D -> r

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, PropagateColor a, W.Transformable b, a ~ t) => Rotated'Go (Optic p g a b a b) t where
  rotated'Go forward backward = iso (propagateColor (runTransform3D forward)) (runTransform3D backward)

instance {-# INCOHERENT #-} (ang ~ Double, x ~ Double, y ~ Double, z ~ Double, Rotated'Go r t) => Rotated'Go (x -> y -> z -> ang -> r) t where
  rotated'Go forward backward x y z ang =
    let rotation = Transform3D (W.rotate (V3 x y z) (mod2pi ang))
        inverse = Transform3D (W.rotate (V3 x y z) (mod2pi (-ang)))
     in rotated'Go (composeTransform3D forward rotation) (composeTransform3D inverse backward)

instance {-# OVERLAPPABLE #-} (ang ~ Double, d ~ Double, Rotated'Go r t) => Rotated'Go (V3 d -> ang -> r) t where
  rotated'Go forward backward v ang =
    let rotation = Transform3D (W.rotate v (mod2pi ang))
        inverse = Transform3D (W.rotate v (mod2pi (-ang)))
     in rotated'Go (composeTransform3D forward rotation) (composeTransform3D inverse backward)

instance {-# OVERLAPPABLE #-} (d ~ Double, Rotated'Go r t) => Rotated'Go (Quaternion d -> r) t where
  rotated'Go forward backward q =
    let angle = acos (q ^. _x)
        rotation = Transform3D (W.rotate (q ^. _yzw) angle)
        inverse = Transform3D (W.rotate (q ^. _yzw) (-angle))
     in rotated'Go (composeTransform3D forward rotation) (composeTransform3D inverse backward)

instance {-# OVERLAPPABLE #-} (v ~ V3, ang ~ Double, Rotated'Go r t) => Rotated'Go (E v -> ang -> r) t where
  rotated'Go forward backward (E e) ang =
    let rotation = Transform3D (W.rotate (0 & e .~ 1) (mod2pi ang))
        inverse = Transform3D (W.rotate (0 & e .~ 1) (mod2pi (-ang)))
     in rotated'Go (composeTransform3D forward rotation) (composeTransform3D inverse backward)

class (W.Transformable t) => RotateGo r t | r -> t where
  rotateGo :: (t -> t) -> (V3 Double -> Double -> t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (PropagateColor a, a' ~ a, a ~ t) => RotateGo (a -> a') t where
  rotateGo acc f = propagateColor acc

instance {-# INCOHERENT #-} (ang ~ Double, x ~ Double, y ~ Double, z ~ Double, RotateGo (r -> s) t) => RotateGo (x -> y -> z -> ang -> r -> s) t where
  rotateGo acc f x y z ang = rotateGo (acc . f (V3 x y z) ang) f

instance {-# OVERLAPPABLE #-} (ang ~ Double, d ~ Double, RotateGo r t) => RotateGo (V3 d -> ang -> r) t where
  rotateGo acc f v ang = rotateGo (acc . f v ang) f

instance {-# OVERLAPPABLE #-} (d ~ Double, RotateGo (r -> s) t) => RotateGo (Quaternion d -> r -> s) t where
  rotateGo acc f q = rotateGo (acc . f (q ^. _yzw) (acos (q ^. _x))) f

instance {-# OVERLAPPABLE #-} (v ~ V3, ang ~ Double, RotateGo r t) => RotateGo (E v -> ang -> r) t where
  rotateGo acc f (E e) ang = rotateGo (acc . f (0 & e .~ 1) (mod2pi ang)) f

class (W.Transformable t) => RotatedGo r t | r -> t where
  rotatedGo :: (t -> t) -> (t -> t) -> r

instance
  {-# OVERLAPPABLE #-}
  ( Profunctor p,
    Functor g,
    PropagateColor a,
    a' ~ a,
    a ~ t
  ) =>
  RotatedGo (Optic' p g a a') t
  where
  rotatedGo forward backward =
    iso
      (propagateColor forward :: a' -> a)
      (propagateColor backward :: a -> a')

-- Keep the final optic endomorphic so composition fixes its intermediate type.
instance
  {-# INCOHERENT #-}
  ( W.Transformable t,
    ang ~ Double,
    x ~ Double,
    y ~ Double,
    z ~ Double,
    a' ~ a,
    RotatedGo (a -> a) t
  ) =>
  RotatedGo (x -> y -> z -> ang -> a -> a') t
  where
  rotatedGo forward backward x y z ang =
    let rotation = W.rotate (V3 x y z) (mod2pi ang)
        inverse = W.rotate (V3 x y z) (mod2pi (-ang))
     in rotatedGo (forward . rotation) (inverse . backward)

instance
  {-# OVERLAPPABLE #-}
  ( W.Transformable t,
    ang ~ Double,
    d ~ Double,
    a' ~ a,
    RotatedGo (a -> a) t
  ) =>
  RotatedGo (V3 d -> ang -> a -> a') t
  where
  rotatedGo forward backward v ang =
    let rotation = W.rotate v (mod2pi ang)
        inverse = W.rotate v (mod2pi (-ang))
     in rotatedGo (forward . rotation) (inverse . backward)

instance
  {-# OVERLAPPABLE #-}
  ( W.Transformable t,
    d ~ Double,
    a' ~ a,
    RotatedGo (a -> a) t
  ) =>
  RotatedGo (Quaternion d -> a -> a') t
  where
  rotatedGo forward backward q =
    let angle = acos (q ^. _x)
        rotation = W.rotate (q ^. _yzw) angle
        inverse = W.rotate (q ^. _yzw) (-angle)
     in rotatedGo (forward . rotation) (inverse . backward)

instance
  {-# OVERLAPPABLE #-}
  ( W.Transformable t,
    v ~ V3,
    ang ~ Double,
    a' ~ a,
    RotatedGo (a -> a) t
  ) =>
  RotatedGo (E v -> ang -> a -> a') t
  where
  rotatedGo forward backward (E e) ang =
    let rotation = W.rotate (0 & e .~ 1) (mod2pi ang)
        inverse = W.rotate (0 & e .~ 1) (mod2pi (-ang))
     in rotatedGo (forward . rotation) (inverse . backward)
