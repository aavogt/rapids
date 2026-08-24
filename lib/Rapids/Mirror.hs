{- HLINT ignore "Eta reduce" -}
module Rapids.Mirror where

import Control.Lens hiding (prism)
import Linear
import Rapids.Color
import qualified Waterfall as W

-- * interface

-- | Reflect across one or more planes through the origin.
--
-- > mirror v3
-- > mirror x y z
-- > mirror ex x
-- > mirror ey y
-- > mirror ez z
mirror :: (MirrorGo r t) => r
mirror = mirrorGo id (propagateColor . W.mirror)

-- | 'mirror' except it also returns the original.
mirrored :: (MirrorGo r t) => r
mirrored = mirrorGo id (\v x -> x + propagateColor (W.mirror v) x)

-- | another way to write @_mirrored ... = 'involuted' (mirror ...)@
_mirrored :: (MirroredGo r t) => r
_mirrored = mirroredOpticGo (id :: t -> t) (id :: t -> t)

-- * implementation

class (W.Transformable t, Num t, PropagateColor t) => MirrorGo r t | r -> t where
  mirrorGo :: (t -> t) -> (V3 Double -> t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (Num t, PropagateColor a, a' ~ a, a ~ t) => MirrorGo (a -> a') t where
  mirrorGo acc f x = acc x

instance {-# OVERLAPPABLE #-} (d ~ Double, MirrorGo r t) => MirrorGo (V3 d -> r) t where
  mirrorGo acc f v = mirrorGo (acc . f v) f

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, MirrorGo r t) => MirrorGo (d -> e -> f -> r) t where
  mirrorGo acc f x y z = mirrorGo (acc . f (V3 x y z)) f

instance {-# OVERLAPPABLE #-} (v ~ V3, MirrorGo r t) => MirrorGo (E v -> r) t where
  mirrorGo acc f (E e) = mirrorGo (acc . f (0 & e .~ 1)) f

class W.Transformable t => MirroredGo r t | r -> t where
  mirroredOpticGo :: (t -> t) -> (t -> t) -> r

-- base case
instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, PropagateColor a, a' ~ a, a ~ t) => MirroredGo (Optic' p g a a') t where
  mirroredOpticGo forward backward = iso (propagateColor forward :: a' -> a) (propagateColor backward :: a -> a')

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, MirroredGo r t) => MirroredGo (d -> e -> f -> r) t where
  mirroredOpticGo forward backward x y z =
    let reflection = W.mirror (V3 x y z)
     in mirroredOpticGo (forward . reflection) (reflection . backward)

instance {-# OVERLAPPABLE #-} (d ~ Double, MirroredGo r t) => MirroredGo (V3 d -> r) t where
  mirroredOpticGo forward backward v =
    let reflection = W.mirror v
     in mirroredOpticGo (forward . reflection) (reflection . backward)

instance {-# OVERLAPPABLE #-} (v ~ V3, MirroredGo r t) => MirroredGo (E v -> r) t where
  mirroredOpticGo forward backward (E e) =
    let reflection = W.mirror (0 & e .~ 1)
     in mirroredOpticGo (forward . reflection) (reflection . backward)
