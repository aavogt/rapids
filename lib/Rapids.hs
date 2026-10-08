{- HLINT ignore "Eta reduce" -}

-- | <https://github.com/aavogt/rapids rapids> simplifies <https://hackage.haskell.org/package/waterfall-cad waterfall-cad> expressions
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
    chamferPath,
    filletPath,
    projectPath,
    section,
    silhouette,

    -- * create 3D

    -- "Rapids.ConvexHull"
    hull,
    pad,
    sweep,
    sweepRuled,
    loft2,

    -- ** predefined solids
    axisTriad,
    module Waterfall.Solids,
    frustum,

    -- ** transforms #affine#

    -- | Each operation takes one or more arguments to specify the direction or amount, and multiple groups can be specified,
    -- so that @mirror ex ey@ is short for @mirror ex . mirror ey@
    --
    -- >translate
    -- >    x y z
    -- >    v3
    -- >    ex x
    -- >    ey y
    -- >    ez z
    --
    -- >mirror
    -- >    v3
    -- >    x y z
    -- >    ex
    -- >    ey
    -- >    ez
    --
    -- >rotate
    -- >  x y z radians
    -- >  v3    radians
    -- >  q
    -- >  ex    radians
    --
    -- >scale
    -- >  v3
    -- >  xyz
    -- >  x y z
    -- >  xy z
    -- >  ex x
    -- >  ey y
    --
    --
    --  'scaled' 'scaled2D' 'mirrored' 'translated' 'translated2D' 'rotated' 'rotatedDeg' include the original Solid,
    --
    -- 'stacked' 'centered' 'lefted' 'righted' include both solids
    --
    --
    --  'scale' 'scale2D' 'mirror' 'translate' 'translate2D' 'rotate' 'rotateDeg' only include the transformed Solid,
    --
    -- 'stack' 'center' 'left' 'right' only return the second (translated) Solid
    --
    --
    --
    -- The following example shows two hexagonal holes symmetric about the x axis.
    --
    --
    -- @cutHole :: Solid -> Solid@
    -- @cutHole x = x - 'translate' 'ex' 5 hex@
    -- @  where hex = 'pad' 1 ('unitPolygon' 6)@
    --
    -- @cutHole@ is a useful idea, but being a function, it limits what it's
    -- callers can do with it. @cutMirroredHoles = _ cutHole@ can't access @hex@
    -- and transform it to make two holes. It's possible to "move the workpiece
    -- instead of the tool" here and 'Control.Lens.Iso.Iso' combines forward and
    -- reverse transformations:
    --
    -- @'scale' 10 1 centeredCylinder '&' '_rotated' ez pi '%~' cutHole@ -- temporarily move the workpiece
    --
    -- @scale 10 1 centeredCylinder - 'rotated' ez pi (translate ex 5 hex)@ -- move the tool
    module Rapids.Transforms,
    module Rapids.AABB.Align,
    axisAlignedBoundingBox,
    aabb,

    -- ** others
    revolution,
    unitSpiral,
    FilletChamfer (..),

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
import Data.Coerce
import Data.Fixed (mod')
import Data.IORef
import Foreign.C
import Rapids.AABB
import Rapids.AABB.Align
import Rapids.AABB.Lens (aabb)
import Rapids.Color
import Rapids.Color.AxisTriad
import Rapids.Color.Query (colorQuery)
import Rapids.ConvexHull
import Rapids.EnvDefaults
import Rapids.IniVal
import Rapids.Num
import Rapids.Offset
import Rapids.Pad
import Rapids.Path
import Rapids.Path.CornerOp
import Rapids.Path.Offset
import Rapids.Path.Project
import Rapids.Reexports
import Rapids.Revolution
import Rapids.Section
import Rapids.Silhouette
import Rapids.Spiral
import Rapids.Statistics
import Rapids.Sweep
import Rapids.ToPath
import Rapids.ToShape
import Rapids.Transforms
import System.Directory
import System.FilePath
import qualified Waterfall as W
import Waterfall.Internal.Path (Path (..))
import Waterfall.Solids hiding (Solid, centerOfMass, emptySolid, momentOfInertia, prism, volume)
import Waterfall.TwoD.Internal.Path2D (Path2D (..))

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

-- | @frustum d1 d2 h@ has a @d1@ diameter cirlein the xy plane (z=0) and a @d2@ diameter in the z=@h@ plane.
frustum :: Double -> Double -> Double -> Solid
frustum d1 d2 h = loft [circle d1, translate ez h (circle d2)]

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
  fillet r solid = filletSolidWithColors (CDouble r) solid
  chamfer r solid = chamferSolidWithColors (CDouble r) solid

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
