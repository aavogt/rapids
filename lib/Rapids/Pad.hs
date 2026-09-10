module Rapids.Pad where

import Data.Fixed (mod')
import qualified Waterfall as W
import Rapids.Color
import Rapids.Num ()
import Rapids.Reexports
import Rapids.ToPath
import Rapids.ToShape
import Rapids.Translate

-- | pad is like freecad PartDesign::Pad. It sweeps a shape, or lofts a uScale2D version
class Pad a where
  pad :: a
  -- ^ pad expressions of type 'ToShape' s => s -> 'Solid'
  --
  -- > pad z = Waterfall.prism
  -- > pad z taperFrac
  -- > pad x y z
  -- > pad x y z taperFrac
  -- > pad v
  -- > pad v taperFrac

instance {-# OVERLAPS #-} (Double ~ double, ToShape shape, Solid ~ solid) => Pad (double -> shape -> solid) where
  pad z = W.sweep (line 0 (V3 0 0 z)) . toShape

instance {-# INCOHERENT #-} (Double ~ double, Double ~ taper, ToShape shape, Solid ~ solid) => Pad (double -> taper -> shape -> solid) where
  pad z taperFrac = pad (V3 0 0 z) taperFrac . toShape

instance {-# INCOHERENT #-} (Double ~ double, ToShape shape, Solid ~ solid) => Pad (V3 double -> shape -> solid) where
  pad xyz = W.sweep (line 0 xyz) . toShape

instance {-# INCOHERENT #-} (Pad (d -> d -> d -> taper -> shape -> solid), Double ~ d, Double ~ taper, ToShape shape, Solid ~ solid) => Pad (V3 d -> taper -> shape -> solid) where
  pad (V3 x y z) taperFrac shape = pad x y z taperFrac shape

instance {-# INCOHERENT #-} (Double ~ x, Double ~ y, Double ~ z, Double ~ taper, ToShape shape, Solid ~ solid) => Pad (x -> y -> z -> shape -> solid) where
  pad x y z = W.sweep (line 0 (V3 x y z)) . toShape

instance {-# INCOHERENT #-} (Double ~ x, Double ~ z, Double ~ y, Double ~ taper, ToShape shape, Solid ~ solid) => Pad (x -> y -> z -> taper -> shape -> solid) where
  pad x y z taperFrac shape =
    unions
      [ loft [fromPath2D q, p]
        | q <- shapePaths (toShape shape),
          let p = translate x y z (fromPath2D (uScale2D taperFrac q))
      ]

-- | @sweep path shape@
--
-- > sweep [0, V3 0 0 1e-3, V3 (-2) 0 (h / 2), V3 0 0 h] (rectangle 3 4)
--
-- the first segment 1e-3 fixes the bottom face orientation
sweep :: (ToPath path, ToShape shape) => path -> shape -> Solid
sweep path shape = W.sweep (toPath path) (toShape shape)
