module Rapids.Scale where

import Control.Lens hiding (prism)
import Linear hiding (scaled)
import Rapids.Color
import Waterfall
import qualified Waterfall as W
import qualified Waterfall.Internal.NearZero as WNZ

-- * interface

-- ** 3D

-- | Scale along one or more directions.
scale :: (ScaleGo r t) => r
scale = scaleGo (id :: t -> t) (propagateColor . W.scale) (propagateColor . W.uScale)

-- | 'scale' except it also returns the original,
-- which is only useful if a direction is negative
scaled :: (Num t, ScaleGo r t) => r
scaled = scaleGo (id :: t -> t) (\v x -> x + propagateColor (W.scale v) x) (\v x -> x + propagateColor (W.uScale v) x)

-- | @_scaled@ produces a type-changing 'Iso' from one or more axis/factor pairs.
_scaled :: (Scaled'Go r t) => r
_scaled = scaled'Go (Transform3D id) (Transform3D id)

-- | @_scaled'@ is the endomorphic, overloaded form of '_scaled'.
_scaled' :: (ScaledOpticGo r t) => r
_scaled' = scaledOpticGo True id id

-- ** 2D

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

class (Transformable t) => Scaled'Go r t | r -> t where
  scaled'Go :: Transform3D -> Transform3D -> r

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, PropagateColor a, Transformable b, a ~ t) => Scaled'Go (Optic p g a b a b) t where
  scaled'Go forward backward = iso (propagateColor (runTransform3D forward)) (runTransform3D backward)

instance {-# OVERLAPPING #-} (v ~ V3, amount ~ Double, Scaled'Go r t) => Scaled'Go (E v -> amount -> r) t where
  scaled'Go forward backward (E e) amount =
    let factors = 1 & e .~ amount
        scaling = Transform3D (W.scale factors)
        inverse = Transform3D (W.scale (1 / factors))
     in scaled'Go (composeTransform3D forward scaling) (composeTransform3D inverse backward)

class (Transformable2D t) => Scaled2D'Go r t | r -> t where
  scaled2D'Go :: Transform2D -> Transform2D -> r

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, Transformable2D a, Transformable2D b, a ~ t) => Scaled2D'Go (Optic p g a b a b) t where
  scaled2D'Go forward backward = iso (runTransform2D forward) (runTransform2D backward)

instance {-# OVERLAPPING #-} (v ~ V2, amount ~ Double, Scaled2D'Go r t) => Scaled2D'Go (E v -> amount -> r) t where
  scaled2D'Go forward backward (E e) amount =
    let factors = 1 & e .~ amount
        scaling = Transform2D (W.scale2D factors)
        inverse = Transform2D (W.scale2D (1 / factors))
     in scaled2D'Go (composeTransform2D forward scaling) (composeTransform2D inverse backward)

-- * implementation

class (PropagateColor t, Transformable t) => ScaleGo r t | r -> t where
  scaleGo :: (t -> t) -> (V3 Double -> t -> t) -> (Double -> t -> t) -> r

class (Transformable2D t) => Scale2DGo r t | r -> t where
  scale2DGo :: (t -> t) -> (V2 Double -> t -> t) -> (Double -> t -> t) -> r

instance {-# INCOHERENT #-} (Transformable2D t) => Scale2DGo (t -> t) t where scale2DGo acc _ _ a = acc a

instance {-# INCOHERENT #-} (Num a, v ~ V2, amt ~ Double, Transformable2D a, a' ~ a, a ~ t) => Scale2DGo (E v -> amt -> a -> a') t where
  scale2DGo acc f g (E e) amt a = scale2DGo (acc . f (1 & e .~ amt)) f g a

instance {-# OVERLAPPABLE #-} (x ~ Double, y ~ Double, Transformable2D a, a' ~ a, a ~ t) => Scale2DGo (x -> y -> a -> a') t where
  scale2DGo acc f g x y a = scale2DGo (acc . f (V2 x y)) f g a

instance {-# OVERLAPPABLE #-} (Num a, Transformable2D a, a' ~ a, a ~ t, Double ~ d) => Scale2DGo (d -> a -> a') t where
  scale2DGo acc f g factor a = scale2DGo (acc . g factor) f g a

instance {-# INCOHERENT #-} (Num t, Transformable t) => ScaleGo (t -> t) t where
  scaleGo acc f g x = acc x

instance {-# INCOHERENT #-} (ScaleGo (t -> t) a, Num a, v ~ V3, amt ~ Double, PropagateColor a, a' ~ a, a ~ t) => ScaleGo (E v -> amt -> a -> a') t where
  scaleGo acc f g (E e) amt a = scaleGo (acc . f (1 & e .~ amt)) f g a

instance {-# INCOHERENT #-} (ScaleGo (t -> t) a, Num a, x ~ Double, y ~ Double, z ~ Double, PropagateColor a, a' ~ a, a ~ t) => ScaleGo (x -> y -> z -> a -> a') t where
  scaleGo acc f g x y z a = scaleGo (acc . f (V3 x y z)) f g a

instance {-# INCOHERENT #-} (ScaleGo (t -> t) a, Num a, xy ~ Double, z ~ Double, PropagateColor a, a' ~ a, a ~ t) => ScaleGo (xy -> z -> a -> a') t where
  scaleGo acc f g xy z a = scaleGo (acc . f (V3 xy xy z)) f g a

instance {-# OVERLAPS #-} (ScaleGo (t -> t) a, Num a, PropagateColor a, a' ~ a, a ~ t, Double ~ d) => ScaleGo (d -> a -> a') t where
  scaleGo acc f g factor a = scaleGo (acc . g factor) f g a

scaledOptic ::
  forall p g a a'.
  (Profunctor p, Functor g, PropagateColor a, a' ~ a) =>
  V3 Double ->
  Maybe (Optic' p g a a')
scaledOptic v
  | any WNZ.nearZero v = Nothing
  | otherwise = Just $ iso (propagateColor (W.scale v) :: a' -> a) (propagateColor (W.scale (1 / v)) :: a -> a')

class (Transformable t) => ScaledOpticGo r t | r -> t where
  scaledOpticGo :: Bool -> (t -> t) -> (t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, PropagateColor a, a' ~ a, a ~ t) => ScaledOpticGo (Optic' p g a a') t where
  scaledOpticGo valid forward backward = iso (propagateColor forward :: a' -> a) (propagateColor backward :: a -> a')

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, ScaledOpticGo r t) => ScaledOpticGo (d -> e -> f -> r) t where
  scaledOpticGo valid forward backward x y z =
    let factors = V3 x y z
        scaling = W.scale factors
        inverse = W.scale (1 / factors)
     in scaledOpticGo (valid && not (any WNZ.nearZero factors)) (forward . scaling) (inverse . backward)

instance {-# INCOHERENT #-} (xy ~ Double, z ~ Double, ScaledOpticGo r t) => ScaledOpticGo (xy -> z -> r) t where
  scaledOpticGo valid forward backward xy z =
    let factors = V3 xy xy z
        scaling = W.scale factors
        inverse = W.scale (1 / factors)
     in scaledOpticGo (valid && not (any WNZ.nearZero factors)) (forward . scaling) (inverse . backward)

instance {-# OVERLAPPABLE #-} (d ~ Double, ScaledOpticGo r t) => ScaledOpticGo (V3 d -> r) t where
  scaledOpticGo valid forward backward factors =
    let scaling = W.scale factors
        inverse = W.scale (1 / factors)
     in scaledOpticGo (valid && not (any WNZ.nearZero factors)) (forward . scaling) (inverse . backward)

instance {-# OVERLAPPABLE #-} (d ~ Double, ScaledOpticGo r t) => ScaledOpticGo (d -> r) t where
  scaledOpticGo valid forward backward factor =
    let scaling = W.uScale factor
        inverse = W.uScale (1 / factor)
     in scaledOpticGo (valid && not (WNZ.nearZero factor)) (forward . scaling) (inverse . backward)

instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, ScaledOpticGo r t) => ScaledOpticGo (E v -> amt -> r) t where
  scaledOpticGo valid forward backward (E e) amt =
    let factors = 1 & e .~ amt
        scaling = W.scale factors
        inverse = W.scale (1 / factors)
     in scaledOpticGo (valid && not (any WNZ.nearZero factors)) (forward . scaling) (inverse . backward)

scaled2DOptic ::
  forall p g a a'.
  (Profunctor p, Functor g, Transformable2D a, a' ~ a) =>
  V2 Double ->
  Optic' p g a a'
scaled2DOptic v = iso (W.scale2D v :: a' -> a) (W.scale2D (1 / v) :: a -> a')

class Scaled2DGo r where
  scaled2DGo :: r

instance {-# INCOHERENT #-} (x ~ Double, y ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2DGo (x -> y -> (Optic' p g a a')) where
  scaled2DGo x y = scaled2DOptic (V2 x y)

instance {-# OVERLAPPABLE #-} (d ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2DGo (V2 d -> (Optic' p g a a')) where
  scaled2DGo v = scaled2DOptic v

instance {-# OVERLAPPABLE #-} (d ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2DGo (d -> (Optic' p g a a')) where
  scaled2DGo xy = scaled2DOptic (V2 xy xy)

instance {-# OVERLAPPABLE #-} (v ~ V2, amt ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2DGo (E v -> amt -> (Optic' p g a a')) where
  scaled2DGo (E e) amt = scaled2DOptic (1 & e .~ amt)
