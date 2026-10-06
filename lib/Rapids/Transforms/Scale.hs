{-# OPTIONS_HADDOCK hide, prune #-}
module Rapids.Transforms.Scale where
import Rapids.Transforms.Scale.Go
import Rapids.Color
import qualified Waterfall as W
import Data.Data
-- *** scale
-- **** 3D

-- | Scale along one or more directions.
scale :: (ScaleGo r t) => r
scale = scaleGo (id :: t -> t) (propagateColor . W.scale) (propagateColor . W.uScale)

-- | 'scale' except it also returns the original,
-- which is mostly useful with a negative amount (-1 mirrors)
scaled :: (Num t, ScaleGo r t) => r
scaled = scaleGo (id :: t -> t) (\v x -> x + propagateColor (W.scale v) x) (\v x -> x + propagateColor (W.uScale v) x)

-- | @_scaled@ produces a type-changing 'Iso' from one or more axis/factor pairs.
_scaled :: (ScaledGo a b False r) => r
_scaled = scaledGo (Proxy @False) mempty

-- | @_scaled'@ is the endomorphic, overloaded form of @_scaled@.
_scaled' :: (ScaledGo a b True r) => r
_scaled' = scaledGo (Proxy @True) mempty

-- **** 2D

-- | like 'scale' for 'Transformable2D'
scale2D :: (Scale2DGo r t) => r
scale2D = scale2DGo (id :: t -> t) W.scale2D W.uScale2D

-- | 'scale2D' except it also returns the original.
scaled2D :: (Num t, Scale2DGo r t) => r
scaled2D = scale2DGo (id :: t -> t) (\v x -> x + W.scale2D v x) (\v x -> x + W.uScale2D v x)

-- | @_scaled2D@ produces a type-changing 'Iso' from one or more axis/factor pairs.
_scaled2D :: (Scaled2DGo a b False r) => r
_scaled2D = scaled2DGo (Proxy @False) mempty

-- | @_scaled2D'@ is the endomorphic, overloaded form of '_scaled2D'.
_scaled2D' :: (Scaled2DGo a b True r) => r
_scaled2D' = scaled2DGo (Proxy @True) mempty

