module Rapids.Translate where

import Control.Lens hiding (prism)
import Linear
import Rapids.Color
import Waterfall (Transformable2D)
import qualified Waterfall as W
import qualified Waterfall.Internal.NearZero as WNZ
-- | Translate a 'Transformable' ( 'Path'/'Solid'/'V3' Double) in a direction
class Translate a where
  -- | @translate@ exressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > translate x y z
  -- > translate (v :: V3 Double)
  -- > translate ex x -- along x axis
  -- > translate ey y -- along y
  -- > translate ez z -- along z
  translate :: a

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, PropagateColor a, a' ~ a) => Translate (d -> e -> f -> a -> a') where
  translate x y z a = propagateColor (W.translate (V3 x y z)) a

instance {-# OVERLAPPABLE #-} (d ~ Double, PropagateColor a, a ~ a') => Translate (V3 d -> a -> a') where
  translate v a = propagateColor (W.translate v) a

-- | Translate a 'Transformable' ( 'Path'/'Solid'/'V3' Double) in a direction
class Translated a where
  -- | @translate@ exressions of type 'Transformable' @a => Iso' a a@ (probably Iso' 'Solid' 'Solid')
  --
  -- > _translated x y z
  -- > _translated (v :: V3 Double)
  -- > _translated ex x -- along x axis
  -- > _translated ey y -- along y
  -- > _translated ez z -- along z
  _translated :: a

instance {-# INCOHERENT #-} (Profunctor p, Functor g, d ~ Double, e ~ Double, f ~ Double, PropagateColor a, a' ~ a) => Translated (d -> e -> f -> Optic' p g a a') where
  _translated x y z = iso (translate x y z :: a' -> a) (translate (-x) (-y) (-z) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, d ~ Double, PropagateColor a, a ~ a') => Translated (V3 d -> Optic' p g a a') where
  _translated v = iso (translate v :: a' -> a) (translate (-v) :: a -> a')

-- | Linear defines 'ex' 'ey' 'ez'
--
-- > transform 'ex' 3 solid
instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, PropagateColor a, a' ~ a) => Translate (E v -> amt -> a -> a') where
  translate (E e) amt a = propagateColor (W.translate (0 & e .~ amt)) a

class Translate2D a where
  translate2D :: a

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, Transformable2D a, a' ~ a) => Translate2D (d -> e -> a -> a') where
  translate2D x y a = W.translate2D (V2 x y) a

instance {-# OVERLAPPABLE #-} (d ~ Double, Transformable2D a, a ~ a') => Translate2D (V2 d -> a -> a') where
  translate2D v a = W.translate2D v a

-- | Linear defines 'ex' 'ey' 'ez'
--
-- > transform 'ex' 3 solid
instance {-# OVERLAPPABLE #-} (v ~ V2, amt ~ Double, Transformable2D a, a' ~ a) => Translate2D (E v -> amt -> a -> a') where
  translate2D (E e) amt a = W.translate2D (0 & e .~ amt) a

class Translated2D a where
  _translated2D :: a

instance {-# INCOHERENT #-} (Profunctor p, Functor g, d ~ Double, e ~ Double, Transformable2D a, a' ~ a) => Translated2D (d -> e -> Optic' p g a a') where
  _translated2D x y = iso (translate2D x y :: a' -> a) (translate2D (-x) (-y) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, d ~ Double, Transformable2D a, a ~ a') => Translated2D (V2 d -> Optic' p g a a') where
  _translated2D v = iso (translate2D v :: a' -> a) (translate2D (-v) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, v ~ V2, amt ~ Double, Transformable2D a, a' ~ a) => Translated2D (E v -> amt -> Optic' p g a a') where
  _translated2D (E e) amt = iso (translate2D (0 & e .~ amt) :: a' -> a) (translate2D (0 & e .~ -amt) :: a -> a')

