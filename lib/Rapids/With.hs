-- | overloading separate from definitions
--
-- sort of like the ToPath / ToShape
module Rapids.With where
import Linear
import Control.Lens


-- | @f = withPlane (\normal point -> _)@
--
-- instead of
--
-- > instance Section (V3 Double -> V3 Double -> [Path])
-- > instance Section (E V3 -> V3 Double -> [Path])
--
-- section = withPlane sectionRaw
--
-- TODO overlapping/incoherent like in Rapids.hs
class WithPlane r s where withPlane :: (V3 Double -> V3 Double -> r) -> s

instance WithPlane s (V3 Double -> V3 Double -> s) where withPlane = id

instance WithPlane s (E V3 -> V3 Double -> s) where withPlane f (E n) = f (0 & n .~ 1)
