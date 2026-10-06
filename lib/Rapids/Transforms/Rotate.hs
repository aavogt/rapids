{-# OPTIONS_HADDOCK hide, prune #-}
module Rapids.Transforms.Rotate where
import Rapids.Transforms.Rotate.Go
import Rapids.Color
import qualified Waterfall as W
import Data.Data

-- *** rotate

-- | Rotate a 'Transformable' by radians around an axis specified in one of these ways:
--
-- > rotate x y z radians :: Transformable a => a -> a
-- > rotate v3    radians
-- > rotate q
-- > rotate ex    radians
rotate :: (RotateGo r t) => r
rotate = rotateGo id \axis angle x -> W.rotate axis (mod2pi angle) x

-- | 'rotate' except it also returns the original
rotated :: (Num t, RotateGo r t) => r
rotated = rotateGo id \axis angle x -> x + W.rotate axis (mod2pi angle) x

-- | Rotate a 'Transformable' by degrees around an axis specified in one of these ways:
--
-- > rotateDeg x y z deg
-- > rotateDeg v3 deg
-- > rotateDeg q  deg -- ignore the quaternion's magnitude
-- > rotateDeg ey deg
rotateDeg :: (RotateGo r t) => r
rotateDeg = rotateGo id \axis angle x -> W.rotate axis (fromDeg angle) x

-- | 'rotatedDeg' is 'rotateDeg' which also adds the original at each step
rotatedDeg :: (Num t, RotateGo r t) => r
rotatedDeg = rotateGo id \axis angle x -> x + W.rotate axis (fromDeg angle) x

-- | @_rotated@ produces a type-changing 'Iso' using the same arguments as 'rotate'.
-- The input is rotated before the operation; its result is rotated back.
_rotated :: (RotatedGo a b False r) => r
_rotated = rotatedGo mod2pi (Proxy @False) mempty mempty

-- | @_rotated'@ is the endomorphic, overloaded form of @_rotated@.
_rotated' :: (RotatedGo a b True r) => r
_rotated' = rotatedGo mod2pi (Proxy @True) mempty mempty

_rotatedDeg :: (RotatedGo a b False r) => r
_rotatedDeg = rotatedGo fromDeg (Proxy @False) mempty mempty

_rotatedDeg' :: (RotatedGo a b True r) => r
_rotatedDeg' = rotatedGo fromDeg (Proxy @True) mempty mempty
