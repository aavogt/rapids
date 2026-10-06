-- | 'rotate', 'rotated', 'rotateDeg', 'rotatedDeg' and the isos @_rotated@ and @_rotated'@
-- accept one of: @x y z radians@, @v3 radians@, @q@, @ex radians@ (and @ey@, @ez@),
-- and several groups of those can follow each other: @rotate ex x ey y@ is @rotate ex x . rotate ey y@
module Rapids.Transforms.Rotate.Go where

import Control.Lens
import Data.Fixed (mod')
import Linear hiding (rotate)
import Rapids.Color
import qualified Waterfall as W
import Data.Data
import Rapids.Transforms.Translate.Go

fromDeg :: Double -> Double
fromDeg a = (a * pi / 180) `mod'` (2 * pi)

mod2pi :: Double -> Double
mod2pi a = a `mod'` (2 * pi)

class RotatedGo a b (c :: Bool) r | r -> a b where
  rotatedGo :: (Double -> Double) -- ^ fromDeg or mod2pi
    -> Proxy c -- ^ are forward and backward types equal
    -> Transform3D -> Transform3D -> r

instance {-# INCOHERENT #-} (Profunctor p, Functor g, PropagateColor a, W.Transformable b, s ~ a, t ~ b, TypesEq c a b) => RotatedGo a b c (Optic p g s t a b) where
  rotatedGo _ _ forward backward = iso (propagateColor (runTransform3D forward)) (runTransform3D backward)

-- instance {-# INCOHERENT #-} (ang ~ Double, x ~ Double, y ~ Double, z ~ Double, RotatedGo c r t) => RotatedGo c (x -> y -> z -> ang -> r) t where
--   rotatedGo f c forward backward x y z ang =
--     let rotation = Transform3D (W.rotate (V3 x y z) (f ang))
--         inverse = Transform3D (W.rotate (V3 x y z) (f (-ang)))
--      in rotatedGo f c (composeTransform3D forward rotation) (composeTransform3D inverse backward)

instance {-# OVERLAPS #-} (ang ~ Double, d ~ Double, RotatedGo a b c r) => RotatedGo a b c (V3 d -> ang -> r) where
  rotatedGo f c forward backward v ang =
    let rotation = Transform3D (W.rotate v (f ang))
        inverse = Transform3D (W.rotate v (f (-ang)))
     in rotatedGo f c (composeTransform3D forward rotation) (composeTransform3D inverse backward)

instance {-# OVERLAPS #-} (d ~ Double, RotatedGo a b c r) => RotatedGo a b c (Quaternion d -> r) where
  rotatedGo f c forward backward q =
    let angle = acos (q ^. _x)
        rotation = Transform3D (W.rotate (q ^. _yzw) angle)
        inverse = Transform3D (W.rotate (q ^. _yzw) (-angle))
     in rotatedGo f c (composeTransform3D forward rotation) (composeTransform3D inverse backward)

instance {-# OVERLAPS #-} (v ~ V3, ang ~ Double, RotatedGo a b c r) => RotatedGo a b c (E v -> ang -> r) where
  rotatedGo f c forward backward (E e) ang =
    let rotation = Transform3D (W.rotate (0 & e .~ 1) (f ang))
        inverse = Transform3D (W.rotate (0 & e .~ 1) (f (-ang)))
     in rotatedGo f c (composeTransform3D forward rotation) (composeTransform3D inverse backward)

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
  -- XXX rotateDeg q already has radians from acos, but f does (* pi/180)
  rotateGo acc f q = rotateGo (acc . f (q ^. _yzw) (acos (q ^. _x))) f

instance {-# OVERLAPPABLE #-} (v ~ V3, ang ~ Double, RotateGo r t) => RotateGo (E v -> ang -> r) t where
  rotateGo acc f (E e) ang = rotateGo (acc . f (0 & e .~ 1) ang) f
