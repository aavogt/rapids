{-# OPTIONS_HADDOCK hide, prune #-}
module Rapids.Transforms.Mirror where
import Rapids.Transforms.Mirror.Go
import Rapids.Color
import qualified Waterfall as W

-- *** mirror

-- | Reflect across one or more planes through the origin.
--
-- > mirror v3
-- >   x y z
-- >   ex x
-- >   ey y
-- >   ez z
mirror :: (MirrorGo r t) => r
mirror = mirrorGo id (propagateColor . W.mirror)

-- | @mirrored ... a = a + mirror ... a@
mirrored :: (MirrorGo r t) => r
mirrored = mirrorGo id (\v x -> x + propagateColor (W.mirror v) x)

-- | @_mirrored ... :: Iso a b a b@ allows the result to have a different type
_mirrored :: (MirroredGo a b r) => r
_mirrored = mirroredGo (Transform3D id) (Transform3D id)

-- | @_mirrored' ... :: Iso a a a a@
_mirrored' :: (MirroredGo a a r) => r
_mirrored' = _mirrored
