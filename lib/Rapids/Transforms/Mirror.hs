{-# OPTIONS_HADDOCK hide, prune #-}
module Rapids.Transforms.Mirror where
import Rapids.Transforms.Mirror.Go
import Rapids.Color
import qualified Waterfall as W
import Data.Data

-- *** mirror

-- | Reflect across one or more planes through the origin.
--
-- > mirror
-- >   v3
-- >   x y z
-- >   ex x
-- >   ey y
-- >   ez z
-- >   solid|v3|path
mirror :: (MirrorGo r t) => r
mirror = mirrorGo id (propagateColor . W.mirror)

-- | @mirrored ... a = a + mirror ... a@ where ... is as in 'mirror'
mirrored :: (MirrorGo r t) => r
mirrored = mirrorGo id (\v x -> x + propagateColor (W.mirror v) x)

-- | @_mirrored ... :: Iso a b a b@ where ... is as in 'mirror'
_mirrored :: (MirroredGo a b False r) => r
_mirrored = mirroredGo (Proxy @False) mempty mempty

-- | @_mirrored' ... :: Iso a a a a@ where ... is as in 'mirror'
_mirrored' :: (MirroredGo a b True r) => r
_mirrored' = mirroredGo (Proxy @True) mempty mempty
