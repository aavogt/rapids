{- HLINT ignore "Eta reduce" -}
module Rapids.Mirror where

import Control.Lens hiding (prism)
import Rapids.Color
import qualified Waterfall as W
import Linear
-- | Reflect across a plane through the origin the normal specified as a V3 Double, E V3
class Mirror a where
  -- | @mirror@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > mirror v3
  -- > mirror x y z
  -- > mirror ex x
  -- > mirror ey y
  -- > mirror ez z
  -- > mirror ex ey ez = mirror ex . mirror ey . mirror ez
  mirror :: a

  -- | @mirrored@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > mirrored v3 = \solid -> mirror v3 solid + solid
  -- > mirrored x y z
  -- > mirrored ex x
  -- > mirrored ey y
  -- > mirrored ez z
  -- > mirrored ex ey ez = mirrored ex . mirrored ey . mirrored ez
  mirrored :: a

instance {-# OVERLAPS #-} (Num s, vd ~ V3 Double, PropagateColor s, s' ~ s) => Mirror (vd -> s -> s') where
  mirror v a = propagateColor (W.mirror v) a
  mirrored v a = propagateColor (W.mirror v) a + a

instance {-# INCOHERENT #-} (Num s, v ~ V3, amt ~ Double, PropagateColor s, s' ~ s) => Mirror (E v -> s -> s') where
  mirror (E e) a = propagateColor (W.mirror (0 & e .~ 1)) a
  mirrored (E e) a = propagateColor (W.mirror (0 & e .~ 1)) a + a

instance {-# INCOHERENT #-} (Num s, v ~ V3, v ~ v', PropagateColor s, s' ~ s) => Mirror (E v -> E v' -> s -> s') where
  mirror (E f) (E g) a =
    let ga = propagateColor (W.mirror (0 & g .~ 1)) a
        fga = propagateColor (W.mirror (0 & f .~ 1)) ga
     in fga
  mirrored (E f) (E g) a =
    let ga = propagateColor (W.mirror (0 & g .~ 1)) a
        fga = propagateColor (W.mirror (0 & f .~ 1)) ga
     in fga + ga + a

instance {-# INCOHERENT #-} (Num s, v ~ V3, v ~ v', v ~ v'', PropagateColor s, s' ~ s) => Mirror (E v -> E v' -> E v'' -> s -> s') where
  mirror (E e) (E f) (E g) a =
    let ga = propagateColor (W.mirror (0 & g .~ 1)) a
        fga = propagateColor (W.mirror (0 & f .~ 1)) ga
        efga = propagateColor (W.mirror (0 & e .~ 1)) fga
     in efga
  mirrored (E e) (E f) (E g) a =
    let ga = propagateColor (W.mirror (0 & g .~ 1)) a
        fga = propagateColor (W.mirror (0 & f .~ 1)) ga
        efga = propagateColor (W.mirror (0 & e .~ 1)) fga
     in efga + fga + ga + a

instance {-# INCOHERENT #-} (Num s, x ~ Double, y ~ Double, z ~ Double, PropagateColor s, s ~ s') => Mirror (x -> y -> z -> s -> s') where
  mirror x y z a = propagateColor (W.mirror (V3 x y z)) a
  mirrored x y z a = propagateColor (W.mirror (V3 x y z)) a + a

class Mirrored a where
  _mirrored :: a

instance {-# OVERLAPS #-} (Num a, Profunctor p, Functor g, vd ~ V3 Double, PropagateColor a, a' ~ a) => Mirrored (vd -> Optic' p g a a') where
  _mirrored v = iso (mirror v :: a' -> a) (mirror v :: a -> a')

instance {-# INCOHERENT #-} (Num a, Profunctor p, Functor g, x ~ Double, y ~ Double, z ~ Double, PropagateColor a, a' ~ a) => Mirrored (x -> y -> z -> Optic' p g a a') where
  _mirrored x y z = iso (mirror x y z :: a' -> a) (mirror x y z :: a -> a')

instance {-# INCOHERENT #-} (Num a, Profunctor p, Functor g, v ~ V3, PropagateColor a, a' ~ a) => Mirrored (E v -> Optic' p g a a') where
  _mirrored (E e) = iso (mirror (E e) :: a' -> a) (mirror (E e) :: a -> a')

instance {-# INCOHERENT #-} (Num a, Profunctor p, Functor g, v ~ V3, v' ~ v, PropagateColor a, a' ~ a) => Mirrored (E v -> E v' -> Optic' p g a a') where
  _mirrored (E f) (E g) = iso (mirror (E f) (E g) :: a' -> a) (mirror (E f) (E g) :: a -> a')

instance {-# INCOHERENT #-} (Num a, Profunctor p, Functor g, v ~ V3, v' ~ v, v'' ~ v, PropagateColor a, a' ~ a) => Mirrored (E v -> E v' -> E v'' -> Optic' p g a a') where
  _mirrored (E e) (E f) (E g) = iso (mirror (E e) (E f) (E g) :: a' -> a) (mirror (E e) (E f) (E g) :: a -> a')

