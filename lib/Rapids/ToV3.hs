module Rapids.ToV3 where

import Rapids.Reexports

-- ToV3s?
class ToV3 a where toV3 :: a -> V3 Double

instance (v ~ V3) => ToV3 (E v) where toV3 (E e) = 0 & e .~ 1
instance {-# INCOHERENT #-} (a ~ V3 Double) => ToV3 a where toV3 = id
