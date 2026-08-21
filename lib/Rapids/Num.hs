module Rapids.Num where

import Control.Applicative
import Control.Lens hiding (prism)
import Control.Monad
import Data.Fixed (mod')
import Data.IORef
import Data.List (tails, sortOn, mapAccumL)
import Data.Maybe
import GHC.Float
import GHC.TypeLits
import Linear hiding (rotate, scaled)
import Numeric.AD
import Numeric.AD.Rank1.Tower (Tower)
import Rapids.Color
import Rapids.Mirror (mirror)
import Rapids.Scale (scale, scale2D)
import Rapids.ConvexHull (Hull (..))
import Rapids.IniVal
import Rapids.Offset (Offset (offset), offsetWithTolerance, tryOffset, tryOffsetWithTolerance)
import Rapids.Path
import Rapids.Path.Project
import Rapids.Revolution (Revolution (..))
import Rapids.Statistics
import Rapids.Section
import Rapids.AABB
import Rapids.ToPath
import Rapids.ToShape
import System.Directory
import System.FilePath
import Waterfall hiding
  ( appendPath2D,
    appendSegment,
    appendSegment2D,
    arc,
    arcRelative,
    arcTo,
    arcVia2D,
    arcVia3D,
    arcViaRelative2D,
    arcViaRelative3D,
    arcViaTo2D,
    arcViaTo3D,
    bezier2D,
    bezier3D,
    bezierRelative2D,
    bezierRelative3D,
    bezierTo2D,
    bezierTo3D,
    axisAlignedBoundingBox,
    centerOfMass,
    closeLoop2D,
    closeLoop3D,
    difference,
    intersection,
    intersections,
    line2D,
    line3D,
    lineRelative2D,
    lineRelative3D,
    lineTo2D,
    lineTo3D,
    mirror,
    momentOfInertia,
    offset,
    offsetWithTolerance,
    pathEndpoints2D,
    pathEndpoints3D,
    pathFrom2D,
    pathFrom3D,
    pathFromTo2D,
    pathFromTo3D,
    pathLength2D,
    pathLength3D,
    repeatLooping,
    reversePath2D,
    reversePath3D,
    revolution,
    rotate,
    scale,
    scale2D,
    splice2D,
    splice3D,
    splitPath2D,
    splitPath3D,
    sweep,
    takePathFraction2D,
    takePathFraction3D,
    translate,
    translate2D,
    tryOffset,
    tryOffsetWithTolerance,
    union,
    unions,
    volume,
    _mirrored,
    _rotated,
    _scaled,
    _scaled2D,
    _translated,
    _translated2D,
  )
import qualified Waterfall as W
import qualified Waterfall.Internal.NearZero as WNZ

-- | needed for `instance Mirrored (_ -> Path -> Path)`
instance Num Path where
  (+) = (<>)
  (-) = error "Num Path missing -"
  (*) = error "Num Path missing *"
  fromInteger = error "Num Path missing fromInteger"
  signum = error "Num Path missing signum"
  abs = error "Num Path missing abs"

-- (-) could remove points, but maybe it needs to defer evaluation ie. store a sign bool (Bool, Path)
-- because x + (0 - x) is supposed to work?

instance Num Solid where
  (-) = difference
  (+) = union
  (*) = intersection
  negate = W.complement
  fromInteger n = scale (fromInteger n) unitCube

  -- \| reflect if the 'centerOfMass' is behind the plane centered at the origin with normal (1,1,1)
  abs x
    | sum (centerOfMass x) < 0 = mirror (1 :: V3 Double) x
    | otherwise = x

  -- \| `abs . signum = signum . abs`
  -- violated because the aabb center of mass can be on the other side of the plane.
  -- consider a solid that's a big sphere at (-1, 0,0) and a small sphere at 2,0,0
  -- abs . signum will not mirror
  -- signum . abs will mirror
  signum = aabbToSolid . fromMaybe (error msg) . axisAlignedBoundingBox
    where
      msg = "Rapids.signum :: Waterfall.Solid->Waterfall.Solid: can't compute axisAlignedBoundingBox"

instance Num Shape where
  (+) = W.union
  (-) = W.difference
  (*) = W.intersection
  fromInteger i = scale2D (fromInteger i) (fromInteger i) unitSquare
  abs = error "instance Num Shape missing abs"
  signum = error "instance Num Shape missing signum"

