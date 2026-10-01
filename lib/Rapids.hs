{- HLINT ignore "Eta reduce" -}

-- | cascade, waterfall, rapids
-- simplify waterfall-cad expressions by complicating the types and type errors
--
-- <#affine affine transforms> and 'pad' take a direction.
-- The direction is specified as either "Linear"'s 'ex' 'ey' 'ez', as a 'V3', one or more 'Double's.
--
-- Many functions are overloaded on their number and type of arguments. In the documentation
-- the following names are used for the arguments, and angle brackets (@\<x\>@) mean an argument is optional:
--
--   * @x y z xy xyz taperFrac taperSlope radians turns amount@ are 'Double's
--   * @v3@ is a @'V3' 'Double'@, @q@ is a @'Quaternion' 'Double'@
--   * @path@ is a 'ToPath' value: @[V2 Double]@, @[V3 Double]@, 'Path2D' or 'Path'
--   * @shape@ is a 'ToShape' value: @[V2 Double]@, 'Path2D', 'Shape' or 'Path', or lists of them
--   * @solid@ is a 'Solid'
module Rapids
  ( -- * IO
    mkStepWriterColor,
    writeSTEPColor,
    mkStepWriter,
    -- | 'readSTEP' and other functions defined in "Waterfall.IO#g:2" are reexported
    iniVal,
    -- | environment variables
    envDefaults,
    whenEnvParseFail,

    -- * create 2D
    rectangle,
    circle,
    offset,
    -- ** specialized 'FilletChamfer'
    chamferPath, filletPath,
    projectPath,
    section,
    silhouette,

    -- * create 3D
    -- "Rapids.ConvexHull"
    hull,
    pad, sweep,
    sweepRuled,
    loft2,

    -- ** predefined solids
    axisTriad,
    module Waterfall.Solids,
    fustrum,

    -- ** affine transforms #affine#

    -- |
    --
    --  'scaled' 'scaled2D' 'mirrored' 'translated' 'translated2D' 'rotated' 'rotatedDeg' include the original Solid,
    -- 'above' 'stacked' 'centered' 'lefted' 'righted' include the left Solid
    --
    --  'scale' 'scale2D' 'mirror' 'translate' 'translate2D' 'rotate' 'rotateDeg' only include the transformed Solid,
    -- 'stack' 'center' 'left' 'right' only move the right Solid
    module Rapids.Scale,
    module Rapids.Mirror,

    -- *** rigid body
    module Rapids.Translate,
    module Rapids.Rotate,
    module Rapids.AABB.Align,
    axisAlignedBoundingBox,
    aabb,

    -- ** others
    revolution,
    unitSpiral,
    FilletChamfer(..),

    -- * consume 3d
    module Rapids.Statistics,
    -- | see also "Rapids.Section"
    colorQuery,

    -- * implementation details
    module Rapids.Num,
    module Rapids.ToPath,
    module Rapids.ToShape,

    -- * reexports
    module Rapids.Color,
    module Rapids.Reexports,
  )
where

import Control.Applicative
import Data.Fixed (mod')
import Data.IORef
import Rapids.AABB
import Rapids.AABB.Align
import Rapids.Color
import Rapids.Color.Query (colorQuery)
import Rapids.ConvexHull
import Rapids.EnvDefaults
import Rapids.IniVal
import Rapids.Mirror
import Rapids.Num
import Rapids.Offset
import Rapids.Pad
import Rapids.Path
import Rapids.Path.Offset
import Rapids.Path.Project
import Rapids.Path.CornerOp
import Rapids.Reexports
import Rapids.Revolution
import Rapids.Rotate
import Rapids.Scale
import Rapids.Section
import Rapids.Silhouette
import Rapids.Spiral
import Rapids.Statistics
import Rapids.ToPath
import Rapids.ToShape
import Rapids.Sweep
import Rapids.Translate
import System.Directory
import System.FilePath
import qualified Waterfall as W
import Waterfall.Solids hiding (Solid, centerOfMass, emptySolid, momentOfInertia, prism, volume)
import Rapids.AABB.Lens (aabb)
import Rapids.Color.AxisTriad
import Data.Coerce
import Waterfall.Internal.Path (Path(..))
import Waterfall.TwoD.Internal.Path2D (Path2D(..))

-- | @main = do write <- mkStepWriter; write solid1; write solid2@
-- writes solid1 to @$(basename $PWD).step@ and solid2 to @$(basename $PWD)0.step@
--
-- so the template needs less renaming
mkStepWriter :: IO (Solid -> IO FilePath)
mkStepWriter = do
  count <- newIORef Nothing
  prefix <- takeBaseName <$> getCurrentDirectory
  return \solid -> do
    count <- atomicModifyIORef count (\a -> (succ <$> a <|> Just 0, a))
    let out = prefix ++ maybe "" show count ++ ".step"
    writeSTEP out solid
    return out

-- | @fustrum d1 d2 h@ has a circle of diameter @d2@ at z=@h@, and another circle of diameter @d1@ at z=0
fustrum :: Double -> Double -> Double -> Solid
fustrum d1 d2 h = loft [circle d1, translate ez h (circle d2)]

-- | 'loft2' does linear interpolation between vertices, whereas 'loft' introduces curvature.
loft2 :: [Path] -> Solid
loft2 [x, y] = propagatePathColors x (loft [x, y])
loft2 (x : y : xs) = propagatePathColors x (loft [x, y]) + loft2 (y : xs)
loft2 [] = mempty

-- | @fillet r@ rounds and @chamfer r@ cuts the edges (of a 'Solid') or corners (of a 'Path', 'Path2D' or 'Shape') by @r@
--
-- > fillet, chamfer :: Double -> solid|path|path2d|shape -> solid|path|path2d|shape
class FilletChamfer a where
  fillet :: Double -> a -> a
  chamfer :: Double -> a -> a

instance FilletChamfer Solid where
  fillet r solid = propagateSolidColors solid (W.roundFillet r solid)
  chamfer r solid = propagateSolidColors solid (W.chamfer r solid)

instance FilletChamfer Path where
  fillet = filletPath
  chamfer = chamferPath

instance FilletChamfer Path2D where
  fillet = coerce filletPath
  chamfer = coerce chamferPath

-- loses topology? perhaps only the skeleton or outerWire?
instance FilletChamfer Shape where
  fillet r = foldMap (\path -> let output = fillet r path in propagatePathColorsToShape (coerce output) (makeShape output)) . shapePaths
  chamfer r = foldMap (\path -> let output = chamfer r path in propagatePathColorsToShape (coerce output) (makeShape output)) . shapePaths
