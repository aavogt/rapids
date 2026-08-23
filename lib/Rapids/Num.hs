module Rapids.Num where

import Data.Maybe
import Rapids.Mirror
import Rapids.Scale
import Rapids.Statistics
import Rapids.AABB
import Linear
import Waterfall (Path, Path2D, Solid, Shape, unitSquare)
import qualified Waterfall as W
import Rapids.Color

-- | needed for `instance Mirrored (_ -> Path -> Path)`
instance Num Path where
  (+) = (<>)
  (-) = error "Num Path missing -"
  (*) = error "Num Path missing *"
  fromInteger = error "Num Path missing fromInteger"
  signum = error "Num Path missing signum"
  abs = error "Num Path missing abs"

-- | needed for `instance Mirrored (_ -> Path -> Path)`
instance Num Path2D where
  (+) = (<>)
  (-) = error "Num Path2D missing -"
  (*) = error "Num Path2D missing *"
  fromInteger = error "Num Path2D missing fromInteger"
  signum = error "Num Path2D missing signum"
  abs = error "Num Path2D missing abs"

-- (-) could remove points, but maybe it needs to defer evaluation ie. store a sign bool (Bool, Path)
-- because x + (0 - x) is supposed to work?

instance Num Solid where
  (-) = difference
  (+) = union
  (*) = intersection
  negate = W.complement
  fromInteger n = scale (fromInteger n) W.unitCube

  -- \| reflect if the 'centerOfMass' is behind the plane centered at the origin with normal (1,1,1)
  abs x
    | sum (centerOfMass x) < 0 = mirror (1 :: V3 Double) x
    | otherwise = x

  -- \| `abs . signum = signum . abs`
  -- violated because the aabb center of mass can be on the other side of the plane.
  -- consider a solid that's a big sphere at (-1, 0,0) and a small sphere at 2,0,0
  -- abs . signum will not mirror
  -- signum . abs will mirror
  signum = W.aabbToSolid . fromMaybe (error msg) . axisAlignedBoundingBox
    where
      msg = "Rapids.signum :: Waterfall.Solid->Waterfall.Solid: can't compute axisAlignedBoundingBox"

instance Num Shape where
  (+) = W.union
  (-) = W.difference
  (*) = W.intersection
  fromInteger i = scale2D (fromInteger i) (fromInteger i) unitSquare
  abs = error "instance Num Shape missing abs"
  signum = error "instance Num Shape missing signum"

