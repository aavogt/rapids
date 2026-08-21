{- HLINT ignore "Eta reduce" -}
module Rapids.Mirror where

import Control.Lens hiding (prism)
import Linear
import Rapids.Color
import qualified Waterfall as W

-- | Reflect across one or more planes through the origin.
class Mirror r where
  -- | @mirror@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > mirror v3
  -- > mirror x y z
  -- > mirror ex x
  -- > mirror ey y
  -- > mirror ez z
  mirror :: r

  -- | @mirrored@ applies each reflection and unions the results with the original.
  mirrored :: r


class (W.Transformable t, Num t, PropagateColor t) => MirrorGo r t | r -> t where
  mirrorGo :: (t -> t) -> r
  mirroredGo :: (t -> t) -> (t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (Num t, PropagateColor a, a' ~ a, a ~ t) => MirrorGo (a -> a') t where
  mirrorGo acc = propagateColor acc
  mirroredGo acc previous a = propagateColor acc a + previous a

mirrorStep :: W.Transformable t => V3 Double -> t -> t
mirrorStep = W.mirror
instance {-# INCOHERENT #-} (Num s, vd ~ V3 Double, PropagateColor s, s' ~ s) => Mirror (vd -> s -> s') where
  mirror v a = propagateColor (W.mirror v) a
  mirrored v a = propagateColor (W.mirror v) a + a

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, MirrorGo r t) => Mirror (d -> e -> f -> r) where
  mirror x y z = mirrorGo (mirrorStep (V3 x y z))
  mirrored x y z = mirroredGo (mirrorStep (V3 x y z)) (propagateColor (id :: t -> t))

instance {-# OVERLAPPABLE #-} (v ~ V3, MirrorGo r t) => Mirror (E v -> r) where
  mirror (E e) = mirrorGo (mirrorStep (0 & e .~ 1))
  mirrored (E e) = mirroredGo (mirrorStep (0 & e .~ 1)) (propagateColor (id :: t -> t))

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, MirrorGo r t) => MirrorGo (d -> e -> f -> r) t where
  mirrorGo acc x y z = mirrorGo (acc . mirrorStep (V3 x y z))
  mirroredGo acc previous x y z =
    let reflection = mirrorStep (V3 x y z)
        next = acc . reflection
        nextPrevious a = previous a + propagateColor acc a
     in mirroredGo next nextPrevious


instance {-# OVERLAPPABLE #-} (v ~ V3, MirrorGo r t) => MirrorGo (E v -> r) t where
  mirrorGo acc (E e) = mirrorGo (acc . mirrorStep (0 & e .~ 1))
  mirroredGo acc previous (E e) =
    let reflection = mirrorStep (0 & e .~ 1)
        next = acc . reflection
        nextPrevious a = previous a + propagateColor acc a
     in mirroredGo next nextPrevious

class Mirrored r where
  _mirrored :: r

instance {-# OVERLAPPABLE #-} (MirroredGo r t) => Mirrored r where
  _mirrored = mirroredOpticGo (id :: t -> t) (id :: t -> t)

class W.Transformable t => MirroredGo r t | r -> t where
  mirroredOpticGo :: (t -> t) -> (t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, PropagateColor a, a' ~ a, a ~ t) => MirroredGo (Optic' p g a a') t where
  mirroredOpticGo forward backward = iso (propagateColor forward :: a' -> a) (propagateColor backward :: a -> a')

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, MirroredGo r t) => MirroredGo (d -> e -> f -> r) t where
  mirroredOpticGo forward backward x y z =
    let reflection = mirrorStep (V3 x y z)
     in mirroredOpticGo (forward . reflection) (reflection . backward)

instance {-# OVERLAPPABLE #-} (d ~ Double, MirroredGo r t) => MirroredGo (V3 d -> r) t where
  mirroredOpticGo forward backward v =
    let reflection = mirrorStep v
     in mirroredOpticGo (forward . reflection) (reflection . backward)

instance {-# OVERLAPPABLE #-} (v ~ V3, MirroredGo r t) => MirroredGo (E v -> r) t where
  mirroredOpticGo forward backward (E e) =
    let reflection = mirrorStep (0 & e .~ 1)
     in mirroredOpticGo (forward . reflection) (reflection . backward)
