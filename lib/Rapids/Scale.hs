module Rapids.Scale where

import Control.Lens hiding (prism)
import Linear hiding (scaled)
import Rapids.Color
import Rapids.ConvexHull (Hull (..))
import Waterfall
import qualified Waterfall as W
import qualified Waterfall.Internal.NearZero as WNZ
-- | Scale x y z axes
class Scale a where
  -- | @scale@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > scale v3
  -- > scale x y z
  -- > scale xy z
  -- > scale ex x
  -- > scale ey y
  --
  -- > v3 :: V3 Double
  -- > x,y,z,xy :: Double
  -- > ex, ey :: E V3
  scale, scaled :: a

instance {-# INCOHERENT #-} (Num a, v ~ V3, amt ~ Double, PropagateColor a, a' ~ a) => Scale (E v -> amt -> a -> a') where
  scale (E e) amt a = propagateColor (W.scale (1 & e .~ amt)) a
  scaled (E e) amt a = propagateColor (W.scale (1 & e .~ amt)) a + a

instance {-# INCOHERENT #-} (Num a, x ~ Double, y ~ Double, z ~ Double, PropagateColor a, a' ~ a) => Scale (x -> y -> z -> a -> a') where
  scale x y z a = propagateColor (W.scale (V3 x y z)) a
  scaled x y z a = propagateColor (W.scale (V3 x y z)) a + a

instance {-# INCOHERENT #-} (Num a, xy ~ Double, z ~ Double, PropagateColor a, a' ~ a) => Scale (xy -> z -> a -> a') where
  scale xy z a = propagateColor (W.scale (V3 xy xy z)) a
  scaled xy z a = propagateColor (W.scale (V3 xy xy z)) a + a

instance {-# OVERLAPS #-} (Num a, PropagateColor a, a' ~ a, Double ~ d) => Scale (d -> a -> a') where
  scale xyz a = propagateColor (W.uScale xyz) a
  scaled xyz a = propagateColor (W.uScale xyz) a + a

-- | Scale x y axes
class Scale2D a where
  -- | @scale2D@ expressions of type 'Transformable2D' @a => a -> a@ (probably 'Shape' -> 'Shape')
  --
  -- > scale2D v2
  -- > scale2D x y z
  -- > scale2D ex x
  -- > scale2D ey y
  scale2D, scaled2D :: a

instance {-# INCOHERENT #-} (Num a, v ~ V2, amt ~ Double, Transformable2D a, a' ~ a) => Scale2D (E v -> amt -> a -> a') where
  scale2D (E e) amt a = W.scale2D (1 & e .~ amt) a
  scaled2D (E e) amt a = W.scale2D (1 & e .~ amt) a + a

instance {-# OVERLAPPABLE #-} (Num a, x ~ Double, y ~ Double, Transformable2D a, a' ~ a) => Scale2D (x -> y -> a -> a') where
  scale2D x y a = W.scale2D (V2 x y) a
  scaled2D x y a = W.scale2D (V2 x y) a + a

instance {-# OVERLAPPABLE #-} (Num a, Transformable2D a, a' ~ a, Double ~ d) => Scale2D (d -> a -> a') where
  scale2D xy a = W.scale2D (V2 xy xy) a
  scaled2D xy a = W.scale2D (V2 xy xy) a + a

scaledOptic ::
  forall p g a a'.
  (Profunctor p, Functor g, PropagateColor a, a' ~ a) =>
  V3 Double ->
  Maybe (Optic' p g a a')
scaledOptic v
  | any WNZ.nearZero v = Nothing
  | otherwise = Just $ iso (propagateColor (W.scale v) :: a' -> a) (propagateColor (W.scale (1 / v)) :: a -> a')

class Scaled a where
  _scaled :: a

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, Profunctor p, Functor g, PropagateColor a, a' ~ a) => Scaled (d -> e -> f -> Maybe (Optic' p g a a')) where
  _scaled x y z = scaledOptic (V3 x y z)

instance {-# OVERLAPPABLE #-} (d ~ Double, Profunctor p, Functor g, PropagateColor a, a' ~ a) => Scaled (V3 d -> Maybe (Optic' p g a a')) where
  _scaled v = scaledOptic v

instance {-# OVERLAPPABLE #-} (xy ~ Double, z ~ Double, Profunctor p, Functor g, PropagateColor a, a' ~ a) => Scaled (xy -> z -> Maybe (Optic' p g a a')) where
  _scaled xy z = scaledOptic (V3 xy xy z)

instance {-# OVERLAPS #-} (d ~ Double, Profunctor p, Functor g, PropagateColor a, a' ~ a) => Scaled (d -> Maybe (Optic' p g a a')) where
  _scaled xyz = scaledOptic (V3 xyz xyz xyz)

instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, Profunctor p, Functor g, PropagateColor a, a' ~ a) => Scaled (E v -> amt -> Maybe (Optic' p g a a')) where
  _scaled (E e) amt = scaledOptic (1 & e .~ amt)

scaled2DOptic ::
  forall p g a a'.
  (Profunctor p, Functor g, Transformable2D a, a' ~ a) =>
  V2 Double ->
  Maybe (Optic' p g a a')
scaled2DOptic v
  | any WNZ.nearZero v = Nothing
  | otherwise = Just $ iso (W.scale2D v :: a' -> a) (W.scale2D (1 / v) :: a -> a')

class Scaled2D a where
  _scaled2D :: a

instance {-# INCOHERENT #-} (x ~ Double, y ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2D (x -> y -> Maybe (Optic' p g a a')) where
  _scaled2D x y = scaled2DOptic (V2 x y)

instance {-# OVERLAPPABLE #-} (d ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2D (V2 d -> Maybe (Optic' p g a a')) where
  _scaled2D v = scaled2DOptic v

instance {-# OVERLAPPABLE #-} (d ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2D (d -> Maybe (Optic' p g a a')) where
  _scaled2D xy = scaled2DOptic (V2 xy xy)

instance {-# OVERLAPPABLE #-} (v ~ V2, amt ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2D (E v -> amt -> Maybe (Optic' p g a a')) where
  _scaled2D (E e) amt = scaled2DOptic (1 & e .~ amt)

