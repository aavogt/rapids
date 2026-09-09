module Rapids.AABB.Lens where

import Rapids.AABB
import Rapids.Num ()
import Rapids.Reexports
import Rapids.Scale
import Rapids.Translate
import Data.Type.Equality (apply)
import qualified Waterfall.Internal.NearZero as WNZ

-- | edit the 'axisAlignedBoundingBox'
aabb :: Lens' Solid (Maybe (V3 Double,V3 Double))
aabb f s =
  let b = axisAlignedBoundingBox s
  in applyAABBTransform s b <$> f b

applyAABBTransform :: Solid -> Maybe (V3 Double, V3 Double) -> Maybe (V3 Double, V3 Double) -> Solid
applyAABBTransform s (Just (a,b)) (Just (c,d)) = s
  & translate (-abCenter)
  & scale (roundTo1 <$> (d - c) / (b - a))
  & translate cdCenter
  where
  abCenter = (a+b)/2
  cdCenter = (c+d)/2
applyAABBTransform _ Nothing (Just cd) = aabbToSolid cd
applyAABBTransform _ _ _ = mempty

roundTo1 :: Double -> Double
roundTo1 x
  | WNZ.nearZero (x - 1) = 1
  | otherwise = x
