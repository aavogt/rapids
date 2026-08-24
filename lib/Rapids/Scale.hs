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
scale = scaleGo False (id :: t -> t)

-- | 'scale' except it also returns the original,
-- which is only useful if a direction is negative
scaled :: (ScaleGo r t) => r
scaled = scaleGo True (id :: t -> t)

_scaled :: (ScaledOpticGo r t) => r
_scaled = scaledOpticGo True id id

-- ** 2D

scale2D :: (Scale2DGo r t) => r
scale2D = scale2DGo False (id :: t -> t)

-- | 'scale2D' except it also returns the original.
scaled2D :: (Scale2DGo r t) => r
scaled2D = scale2DGo True (id :: t -> t)

_scaled2D :: (Scaled2DGo r) => r
_scaled2D = scaled2DGo

-- * implementation

class (Transformable t) => ScaleGo r t | r -> t where
  scaleGo :: Bool -> (t -> t) -> r

scaleResult keep acc transform original =
  let result = propagateColor (acc . transform) original
   in if keep then original + result else result

class Transformable2D t => Scale2DGo r t | r -> t where
  scale2DGo :: Bool -> (t -> t) -> r

scale2DResult keep acc transform original =
  let result = acc (transform original)
   in if keep then original + result else result

instance {-# INCOHERENT #-} (Num a, v ~ V2, amt ~ Double, Transformable2D a, a' ~ a, a ~ t) => Scale2DGo (E v -> amt -> a -> a') t where
  scale2DGo keep acc (E e) amt a = scale2DResult keep acc (W.scale2D (1 & e .~ amt)) a

instance {-# OVERLAPPABLE #-} (Num a, x ~ Double, y ~ Double, Transformable2D a, a' ~ a, a ~ t) => Scale2DGo (x -> y -> a -> a') t where
  scale2DGo keep acc x y a = scale2DResult keep acc (W.scale2D (V2 x y)) a

instance {-# OVERLAPPABLE #-} (Num a, Transformable2D a, a' ~ a, a ~ t, Double ~ d) => Scale2DGo (d -> a -> a') t where
  scale2DGo keep acc factor a = scale2DResult keep acc (W.scale2D (V2 factor factor)) a

instance {-# INCOHERENT #-} (Num t, Transformable t) => ScaleGo (t -> t) t where
  scaleGo keep acc x = if keep then x + acc x else acc x

instance {-# INCOHERENT #-} (ScaleGo (t -> t) a, Num a, v ~ V3, amt ~ Double, PropagateColor a, a' ~ a, a ~ t) => ScaleGo (E v -> amt -> a -> a') t where
  scaleGo keep acc (E e) amt a = scaleGo keep (acc . W.scale (1 & e .~ amt)) a

instance {-# INCOHERENT #-} (ScaleGo (t -> t) a, Num a, x ~ Double, y ~ Double, z ~ Double, PropagateColor a, a' ~ a, a ~ t) => ScaleGo (x -> y -> z -> a -> a') t where
  scaleGo keep acc x y z a = scaleGo keep (acc . W.scale (V3 x y z)) a

instance {-# INCOHERENT #-} (ScaleGo (t -> t) a, Num a, xy ~ Double, z ~ Double, PropagateColor a, a' ~ a, a ~ t) => ScaleGo (xy -> z -> a -> a') t where
  scaleGo keep acc xy z a = scaleGo keep (acc . W.scale (V3 xy xy z)) a

instance {-# OVERLAPS #-} (ScaleGo (t -> t) a, Num a, PropagateColor a, a' ~ a, a ~ t, Double ~ d) => ScaleGo (d -> a -> a') t where
  scaleGo keep acc factor a = scaleGo keep (acc . W.uScale factor) a


scaledOptic ::
  forall p g a a'.
  (Profunctor p, Functor g, PropagateColor a, a' ~ a) =>
  V3 Double ->
  Maybe (Optic' p g a a')
scaledOptic v
  | any WNZ.nearZero v = Nothing
  | otherwise = Just $ iso (propagateColor (W.scale v) :: a' -> a) (propagateColor (W.scale (1 / v)) :: a -> a')

class Transformable t => ScaledOpticGo r t | r -> t where
  scaledOpticGo :: Bool -> (t -> t) -> (t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, PropagateColor a, a' ~ a, a ~ t) => ScaledOpticGo (Maybe (Optic' p g a a')) t where
  scaledOpticGo valid forward backward
    | valid = Just $ iso (propagateColor forward :: a' -> a) (propagateColor backward :: a -> a')
    | otherwise = Nothing

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
  Maybe (Optic' p g a a')
scaled2DOptic v
  | any WNZ.nearZero v = Nothing
  | otherwise = Just $ iso (W.scale2D v :: a' -> a) (W.scale2D (1 / v) :: a -> a')

class Scaled2DGo r where
  scaled2DGo :: r

instance {-# INCOHERENT #-} (x ~ Double, y ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2DGo (x -> y -> Maybe (Optic' p g a a')) where
  scaled2DGo x y = scaled2DOptic (V2 x y)

instance {-# OVERLAPPABLE #-} (d ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2DGo (V2 d -> Maybe (Optic' p g a a')) where
  scaled2DGo v = scaled2DOptic v

instance {-# OVERLAPPABLE #-} (d ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2DGo (d -> Maybe (Optic' p g a a')) where
  scaled2DGo xy = scaled2DOptic (V2 xy xy)

instance {-# OVERLAPPABLE #-} (v ~ V2, amt ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2DGo (E v -> amt -> Maybe (Optic' p g a a')) where
  scaled2DGo (E e) amt = scaled2DOptic (1 & e .~ amt)

