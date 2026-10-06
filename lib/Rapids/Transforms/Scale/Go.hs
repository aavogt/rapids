-- | 'scale', 'scaled', and the isos @_scaled@ and @_scaled'@
-- accept one of: @v3@, @xyz@, @x y z@, @xy z@, @ex x@ (and @ey@, @ez@),
-- and several groups of those can follow each other. The @2D@ versions are for 'Shape' and 'Path2D'.
module Rapids.Transforms.Scale.Go where

import Control.Lens hiding (prism)
import Data.Data
import Linear hiding (scaled)
import Rapids.Color
import Rapids.Transforms.Translate.Go
import Waterfall
import qualified Waterfall as W

class ScaledGo a b (c :: Bool) r | r -> a b where
  scaledGo :: Proxy c -> Scaling V3 -> r

-- base case
instance
  {-# INCOHERENT #-}
  (Profunctor p, Functor g, PropagateColor a, Transformable b, s ~ a, t ~ b, TypesEq c a b) =>
  ScaledGo a b c (Optic p g s t a b)
  where
  scaledGo _ = \case
    Scaling (Left a) -> iso (propagateColor (scale a)) (propagateColor (scale (1 / a)))
    Scaling (Right a) -> iso (propagateColor (uScale a)) (propagateColor (uScale (1 / a)))

instance {-# OVERLAPS #-} (v ~ V3, amount ~ Double, ScaledGo a b c r) => ScaledGo a b c (E v -> amount -> r) where
  scaledGo c v (E e) amount = scaledGo c (onV3 v $ e *~ amount)

instance {-# OVERLAPS #-} (d ~ Double, ScaledGo a b c r) => ScaledGo a b c (V3 d -> r) where
  scaledGo c v w = scaledGo c (onV3 v (* w))

instance {-# INCOHERENT #-} (x ~ Double, y ~ Double, z ~ Double, ScaledGo a b c r) => ScaledGo a b c (x -> y -> z -> r) where
  scaledGo c v x y z = scaledGo c (v <> scalingV3 x y z)

-- instance {-# OVERLAPS #-} (ScaledGo a b c r) => ScaledGo a b c (Double -> Double -> r) where
--   scaledGo c v xy z = scaledGo c (v <> scalingV3 xy xy z)
--
-- instance {-# OVERLAPS #-} (ScaledGo a b c r) => ScaledGo a b c (Double -> r) where
--   scaledGo c v xyz = scaledGo c (onU v (* xyz))

class Scaled2DGo a b (c :: Bool) r | r -> a b where
  scaled2DGo :: Proxy c -> Scaling V2 -> r

-- base case
instance
  {-# INCOHERENT #-}
  (Profunctor p, Functor g, Transformable2D a, Transformable2D b, s ~ a, t ~ b, TypesEq c a b) =>
  Scaled2DGo a b c (Optic p g s t a b)
  where
  scaled2DGo _ = \case
    Scaling (Left a) -> iso (scale2D a) (scale2D (1 / a))
    Scaling (Right a) -> iso (uScale2D a) (uScale2D (1 / a))

instance {-# OVERLAPS #-} (v ~ V2, amount ~ Double, Scaled2DGo a b c r) => Scaled2DGo a b c (E v -> amount -> r) where
  scaled2DGo c v (E e) amount = scaled2DGo c (onV2 v $ e *~ amount)

instance {-# OVERLAPS #-} (Scaled2DGo a b c r) => Scaled2DGo a b c (Double -> Double -> r) where
  scaled2DGo c v x y = scaled2DGo c (v <> scalingV2 x y)

instance {-# OVERLAPS #-} (Scaled2DGo a b c r) => Scaled2DGo a b c (Double -> r) where
  scaled2DGo c v xy = scaled2DGo c (onU v (* xy))

newtype Scaling v = Scaling (Either (v Double) Double)

instance (Functor v, Num (v Double)) => Monoid (Scaling v) where mempty = Scaling (Right 1)

instance (Functor v, Num (v Double)) => Semigroup (Scaling v) where
  Scaling (Left a) <> Scaling (Left b) = Scaling (Left (a * b))
  Scaling (Right a) <> Scaling (Right b) = Scaling (Right (a * b))
  Scaling (Left a) <> Scaling (Right b) = Scaling (Left (fmap (* b) a))
  Scaling (Right a) <> Scaling (Left b) = Scaling (Left (fmap (a *) b))

scalingU :: Double -> Scaling v
scalingU u = Scaling (Right u)

scalingV3 :: Double -> Double -> Double -> Scaling V3
scalingV3 x y z = Scaling (Left (V3 x y z))

scalingV2 :: Double -> Double -> Scaling V2
scalingV2 x y = Scaling (Left (V2 x y))

onV3 :: Scaling V3 -> (V3 Double -> V3 Double) -> Scaling V3
onV3 (Scaling v) k = Scaling $ Left $ k $ either id (\u -> V3 u u u) v

onV2 :: Scaling V2 -> (V2 Double -> V2 Double) -> Scaling V2
onV2 (Scaling v) k = Scaling $ Left $ k $ either id (\u -> V2 u u) v

onU :: (Functor v) => Scaling v -> (Double -> Double) -> Scaling v
onU (Scaling (Left v)) k = Scaling $ Left (fmap k v)
onU (Scaling (Right v)) k = Scaling $ Right (k v)

class (PropagateColor t, Transformable t) => ScaleGo r t | r -> t where
  scaleGo :: (t -> t) -> (V3 Double -> t -> t) -> (Double -> t -> t) -> r

class (Transformable2D t) => Scale2DGo r t | r -> t where
  scale2DGo :: (t -> t) -> (V2 Double -> t -> t) -> (Double -> t -> t) -> r

instance {-# INCOHERENT #-} (Transformable2D t) => Scale2DGo (t -> t) t where scale2DGo acc _ _ a = acc a

instance {-# INCOHERENT #-} (Num a, v ~ V2, amt ~ Double, Transformable2D a, a' ~ a, a ~ t) => Scale2DGo (E v -> amt -> a -> a') t where
  scale2DGo acc f g (E e) amt a = scale2DGo (acc . f (1 & e .~ amt)) f g a

instance {-# INCOHERENT #-} (x ~ Double, Transformable2D a, a' ~ a, a ~ t) => Scale2DGo (V2 x -> a -> a') t where
  scale2DGo acc f g xy a = scale2DGo (acc . f xy) f g a

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

instance {-# INCOHERENT #-} (ScaleGo (t -> t) a, Num a, d ~ Double, PropagateColor a, a' ~ a, a ~ t) => ScaleGo (V3 d -> a -> a') t where
  scaleGo acc f g v a = scaleGo (acc . f v) f g a

instance {-# OVERLAPS #-} (ScaleGo (t -> t) a, Num a, PropagateColor a, a' ~ a, a ~ t, Double ~ d) => ScaleGo (d -> a -> a') t where
  scaleGo acc f g factor a = scaleGo (acc . g factor) f g a
