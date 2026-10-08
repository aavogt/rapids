{-# OPTIONS_HADDOCK hide, prune #-}
module Rapids.Transforms.Scale where
import Rapids.Transforms.Scale.Go
import Rapids.Color
import qualified Waterfall as W
import Data.Data
-- *** scale
-- **** 3D

-- | Scale by one or more vectors
--
-- > scale
-- >  ez z
-- >  x y z
-- >  xy z
-- >  xyz
-- >  v3
-- >  solid|path|v3
scale :: (ScaleGo r t) => r
scale = scaleGo (id :: t -> t) (propagateColor . W.scale) (propagateColor . W.uScale)

-- | @scaled ... x = x + scale ...@
-- which is mostly useful with a negative amount (-1 mirrors)
scaled :: (Num t, ScaleGo r t) => r
scaled = scaleGo (id :: t -> t) (\v x -> x + propagateColor (W.scale v) x) (\v x -> x + propagateColor (W.uScale v) x)

-- | forward and backwards with different types
--
-- > _scaled
-- >   ez z
-- >   x y z
-- >   v3
-- >    :: Iso a b a b
_scaled :: (ScaledGo a b False r) => r
_scaled = scaledGo (Proxy @False) mempty

-- | forward and backwards with the same types
--
-- > _scaled'
-- >   ez z
-- >   x y z
-- >   v3
-- >    :: Iso a a a a
-- >    :: Iso' a a     -- alternative
_scaled' :: (ScaledGo a b True r) => r
_scaled' = scaledGo (Proxy @True) mempty

-- **** 2D

-- |
--
-- > scale
-- >  ey y
-- >  x y
-- >  v2
-- >  xy
-- >  path2d|shape|v2
scale2D :: (Scale2DGo r t) => r
scale2D = scale2DGo (id :: t -> t) W.scale2D W.uScale2D

-- | @scaled2D ... x = x + scale2D ... x@
-- which is mostly useful with a negative amount (-1 mirrors)
scaled2D :: (Num t, Scale2DGo r t) => r
scaled2D = scale2DGo (id :: t -> t) (\v x -> x + W.scale2D v x) (\v x -> x + W.uScale2D v x)

-- | forwards and backwards with different types
--
-- > _scaled2D
-- >   ey y
-- >   x y
-- >   v2
-- >    :: Iso a b a b
_scaled2D :: (Scaled2DGo a b False r) => r
_scaled2D = scaled2DGo (Proxy @False) mempty

-- | forwards and backwards with the same types
--
-- > _scaled2D'
-- >   ey y
-- >   x y
-- >   v2
-- >    :: Iso a a a a
-- >    :: Iso' a a     -- alternative
_scaled2D' :: (Scaled2DGo a b True r) => r
_scaled2D' = scaled2DGo (Proxy @True) mempty

