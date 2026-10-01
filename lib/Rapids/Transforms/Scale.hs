{-# OPTIONS_HADDOCK hide, prune #-}
module Rapids.Transforms.Scale where
import Rapids.Transforms.Scale.Go
import Control.Lens hiding (prism)
import Linear hiding (scaled)
import Rapids.Color
import Waterfall
import qualified Waterfall as W
import qualified Waterfall.Internal.NearZero as WNZ
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
_scaled :: (Scaled'Go r t) => r
_scaled = scaled'Go (Transform3D id) (Transform3D id)

-- | @_scaled'@ is the endomorphic, overloaded form of @_scaled@.
_scaled' :: (ScaledOpticGo r t) => r
_scaled' = scaledOpticGo True id id

-- **** 2D

-- | like 'scale' for 'Transformable2D'
scale2D :: (Scale2DGo r t) => r
scale2D = scale2DGo (id :: t -> t) W.scale2D W.uScale2D

-- | 'scale2D' except it also returns the original.
scaled2D :: (Num t, Scale2DGo r t) => r
scaled2D = scale2DGo (id :: t -> t) (\v x -> x + W.scale2D v x) (\v x -> x + W.uScale2D v x)

-- | @_scaled2D@ produces a type-changing 'Iso' from one or more axis/factor pairs.
_scaled2D :: (Scaled2D'Go r t) => r
_scaled2D = scaled2D'Go (Transform2D id) (Transform2D id)

-- | @_scaled2D'@ is the endomorphic, overloaded form of '_scaled2D'.
_scaled2D' :: (Scaled2DGo r) => r
_scaled2D' = scaled2DGo

