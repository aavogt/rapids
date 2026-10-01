{- HLINT ignore "Eta reduce" -}
-- | 'mirror', 'mirrored', and the isos @_mirrored@ and @_mirrored'@
-- accept one of: @v3@, @x y z@, @ex@ (and @ey@, @ez@).
-- Several groups of those can follow each other.
module Rapids.Transforms.Mirror.Go where

import Control.Lens hiding (prism)
import Linear
import Rapids.Color
import qualified Waterfall as W

class MirroredGo a b r | r -> a b where
  mirroredGo :: Transform3D -> Transform3D -> r

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, PropagateColor a, W.Transformable b, a ~ t) => MirroredGo a b (Optic p g a b a b) where
  mirroredGo forward backward = iso (propagateColor (runTransform3D forward)) (runTransform3D backward)

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, MirroredGo a b r) => MirroredGo a b (d -> e -> f -> r) where
  mirroredGo forward backward x y z =
    let reflection = Transform3D (W.mirror (V3 x y z))
     in mirroredGo (composeTransform3D forward reflection) (composeTransform3D reflection backward)

instance {-# OVERLAPPABLE #-} (d ~ Double, MirroredGo a b r) => MirroredGo a b (V3 d -> r) where
  mirroredGo forward backward v =
    let reflection = Transform3D (W.mirror v)
     in mirroredGo (composeTransform3D forward reflection) (composeTransform3D reflection backward)

instance {-# OVERLAPPABLE #-} (v ~ V3, MirroredGo a b r) => MirroredGo a b (E v -> r) where
  mirroredGo forward backward (E e) =
    let reflection = Transform3D (W.mirror (0 & e .~ 1))
     in mirroredGo (composeTransform3D forward reflection) (composeTransform3D reflection backward)

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
