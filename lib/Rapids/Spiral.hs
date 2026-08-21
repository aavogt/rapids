{- HLINT ignore "Eta reduce" -}
module Rapids.Spiral where

import Data.Fixed (mod')
import Data.List (tails, sortOn, mapAccumL)
import GHC.Float
import Linear
import Numeric.AD
import Numeric.AD.Rank1.Tower (Tower)
import Rapids.Revolution (Revolution (..))
import Rapids.ToPath
import Waterfall

class SpiralPath a where
  unitSpiralPath :: a
  -- ^
  -- > unitSpiral :: Double -> Double -> Path
  -- > unitSpiral :: Double           -> Path
  --
  -- > unitSpiral turns taperSlope :: Path
  -- > unitSprial turns            :: Path

instance {-# OVERLAPS #-} (turns ~ Double, taperSlope ~ Double, path ~ [V3 Double]) => SpiralPath (turns -> taperSlope -> path) where
  unitSpiralPath turns taperSlope =
    [ unitSpiralPoints taperSlope th
      | let fractionalTurn
              | nearZero ((2 * turns) - fromIntegral (floor (2 * turns))) = [] -- is that the right tolerance?
              | otherwise = [arcsPerHalfTurn * 4 * turns],
        th <- map ((/ arcsPerHalfTurn) . (/ 2) . (pi *)) $ map fromIntegral [0 .. floor (arcsPerHalfTurn * 4 * turns)] ++ fractionalTurn
    ]
    where
      arcsPerHalfTurn = 4

unitSpiralPoints taperSlope th = V3 ((1 - taperSlope * z) * sin th) ((1 - taperSlope * z) * cos th) z
  where
    z = th / 2 / pi

instance (turns ~ Double, path ~ [V3 Double]) => SpiralPath (turns -> path) where
  unitSpiralPath turns = unitSpiralPath turns 0

class UnitSpiral a where
  -- | r=1, pitch=1
  --
  -- > scale r r pitch $ unitSpiral turns taperSlope $ rectangle w h
  unitSpiral :: a

instance {-# INCOHERENT #-} (turns ~ Double, taper ~ Double, ToPath profile, Solid ~ solid) => UnitSpiral (turns -> taper -> profile -> solid) where
  unitSpiral turns taperSlope profile = unitSpiral1 turns taperSlope (toPath profile)

unitSpiral1 :: Double -> Double -> Path -> Solid
unitSpiral1 turns taperSlope sh =
    loft
      [ spiralFrame taperSlope th sh
        | let fractionalTurn
                | nearZero (2 * turns - fromIntegral (floorDouble (2 * turns))) = []
                | otherwise = [2 * turns - fromIntegral (floorDouble (2 * turns))],
          let nperhalfturn = 5, -- may need increasing or make extra turns and cut extras out
          n <- map fromIntegral [0 .. floor nperhalfturn * floorDouble (2 * turns)] ++ fractionalTurn,
          let th = pi * n / nperhalfturn
      ]

instance {-# OVERLAPPABLE #-} UnitSpiral (turns -> Double -> profile -> solid) => UnitSpiral (turns -> profile -> solid) where
  unitSpiral turns sh = unitSpiral turns (0 :: Double) sh

spiralFrame :: (Transformable t) => Double -> Double -> t -> t
spiralFrame taperSlope theta = frenetFrame (unitSpiralPoints (auto taperSlope)) theta

-- | place a solid/shape in the Frenet frame
--
-- TODO: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/Computation-of-rotation-minimizing-frames.pdf
frenetFrame :: (Transformable solid) => (forall s. AD s (Tower Double) -> V3 (AD s (Tower Double))) -> Double -> solid -> solid
frenetFrame curve t =
  let (v : tangent : normal : _) = transposeV3List $ diffs0F curve t
      binormal = normalize $ cross tangent normal
      dummy = V3 0 0 1 -- all zeroes would be better, but that throws Standard_ConstructionError
      m = transpose $ V4 (normalize normal) binormal dummy v
   in matTransform m

-- sequenceA zipList ish
transposeV3List :: V3 [a] -> [V3 a]
transposeV3List (V3 a b c) = zipWith3 V3 a b c

