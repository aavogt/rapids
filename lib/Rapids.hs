{- HLINT ignore "Eta reduce" -}

-- | cascade, waterfall, rapids
-- simplify waterfall-cad expressions by complicating the types and type errors
--
-- Examples use
--
-- > x, y, z :: Double
-- > v :: V3 Double
--
-- pragmatic instance Num Solid
--
--  - + union
--  - - cut
--  - * intersection
--  - abs applies 'mirror (V3 1 1 1)' to move the 'centerOfMass'
--  - fromInteger cube
--  - signum = 'aabbToSolid' . 'axisAlignedBoundingBox' :: 'Solid' -> Solid
module Rapids
  ( module Rapids,
    module Rapids.Color,
    module Rapids.ConvexHull,
    module Rapids.IniVal,
    module Rapids.Path,
    module Rapids.Section,
    module Rapids.Statistics,
    module Rapids.Revolution,
    module Rapids.Offset,
    module Rapids.ToPath,
    module Rapids.ToShape,
    module Linear,
    module Control.Lens,
    module Waterfall,
    projectPath,
  )
where

import Control.Applicative
import Control.Lens hiding (prism)
import Control.Monad
import Data.Fixed (mod')
import Data.IORef
import Data.List (tails)
import Data.Maybe
import GHC.Float
import GHC.TypeLits
import Linear hiding (rotate, scaled)
import Numeric.AD
import Numeric.AD.Rank1.Tower (Tower)
import Rapids.Color
import Rapids.ConvexHull (Hull (..))
import Rapids.IniVal
import Rapids.Offset (Offset (offset), offsetWithTolerance, tryOffset, tryOffsetWithTolerance)
import Rapids.Path
import Rapids.Path.Project
import Rapids.Revolution (Revolution (..))
import Rapids.Section (section, sectionPerimeter)
import Rapids.Statistics
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

-- | @main = do write <- mkStepWriter; write solid1; write solid2@
-- writes solid1 to $(basename `pwd`).step and solid2 to $(basename `pwd`)0.step
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

-- | Translate a 'Transformable' ( 'Path'/'Solid'/'V3' Double) in a direction
class Translate a where
  -- | @translate@ exressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > translate x y z
  -- > translate (v :: V3 Double)
  -- > translate ex x -- along x axis
  -- > translate ey y -- along y
  -- > translate ez z -- along z
  translate :: a

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, PropagateColor a, a' ~ a) => Translate (d -> e -> f -> a -> a') where
  translate x y z a = propagateColor (W.translate (V3 x y z)) a

instance {-# OVERLAPPABLE #-} (d ~ Double, PropagateColor a, a ~ a') => Translate (V3 d -> a -> a') where
  translate v a = propagateColor (W.translate v) a

-- | Translate a 'Transformable' ( 'Path'/'Solid'/'V3' Double) in a direction
class Translated a where
  -- | @translate@ exressions of type 'Transformable' @a => Iso' a a@ (probably Iso' 'Solid' 'Solid')
  --
  -- > _translated x y z
  -- > _translated (v :: V3 Double)
  -- > _translated ex x -- along x axis
  -- > _translated ey y -- along y
  -- > _translated ez z -- along z
  _translated :: a

instance {-# INCOHERENT #-} (Profunctor p, Functor g, d ~ Double, e ~ Double, f ~ Double, PropagateColor a, a' ~ a) => Translated (d -> e -> f -> Optic' p g a a') where
  _translated x y z = iso (translate x y z :: a' -> a) (translate (-x) (-y) (-z) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, d ~ Double, PropagateColor a, a ~ a') => Translated (V3 d -> Optic' p g a a') where
  _translated v = iso (translate v :: a' -> a) (translate (-v) :: a -> a')

-- | Linear defines 'ex' 'ey' 'ez'
--
-- > transform 'ex' 3 solid
instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, PropagateColor a, a' ~ a) => Translate (E v -> amt -> a -> a') where
  translate (E e) amt a = propagateColor (W.translate (0 & e .~ amt)) a

class Translate2D a where
  translate2D :: a

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, Transformable2D a, a' ~ a) => Translate2D (d -> e -> a -> a') where
  translate2D x y a = W.translate2D (V2 x y) a

instance {-# OVERLAPPABLE #-} (d ~ Double, Transformable2D a, a ~ a') => Translate2D (V2 d -> a -> a') where
  translate2D v a = W.translate2D v a

-- | Linear defines 'ex' 'ey' 'ez'
--
-- > transform 'ex' 3 solid
instance {-# OVERLAPPABLE #-} (v ~ V2, amt ~ Double, Transformable2D a, a' ~ a) => Translate2D (E v -> amt -> a -> a') where
  translate2D (E e) amt a = W.translate2D (0 & e .~ amt) a

class Translated2D a where
  _translated2D :: a

instance {-# INCOHERENT #-} (Profunctor p, Functor g, d ~ Double, e ~ Double, Transformable2D a, a' ~ a) => Translated2D (d -> e -> Optic' p g a a') where
  _translated2D x y = iso (translate2D x y :: a' -> a) (translate2D (-x) (-y) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, d ~ Double, Transformable2D a, a ~ a') => Translated2D (V2 d -> Optic' p g a a') where
  _translated2D v = iso (translate2D v :: a' -> a) (translate2D (-v) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, v ~ V2, amt ~ Double, Transformable2D a, a' ~ a) => Translated2D (E v -> amt -> Optic' p g a a') where
  _translated2D (E e) amt = iso (translate2D (0 & e .~ amt) :: a' -> a) (translate2D (0 & e .~ -amt) :: a -> a')

-- | Rotate a 'Transformable' by radians around an axis specified in one of these ways:
class Rotate a where
  -- | @rotate@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > rotate x y z rad
  -- > rotate v     rad
  -- > rotate q
  -- > rotate ex    rad
  rotate :: a

instance {-# INCOHERENT #-} (x ~ Double, y ~ Double, z ~ Double, ang ~ Double, PropagateColor a, a' ~ a) => Rotate (x -> y -> z -> ang -> a -> a') where
  rotate x y z ang a = propagateColor (W.rotate (V3 x y z) (mod2pi ang)) a

mod2pi :: Double -> Double
mod2pi a = a `mod'` (2 * pi)

instance {-# OVERLAPPABLE #-} (d ~ Double, ang ~ Double, PropagateColor a, a' ~ a) => Rotate (V3 d -> ang -> a -> a') where
  rotate v ang a = propagateColor (W.rotate v (mod2pi ang)) a

instance {-# OVERLAPPABLE #-} (d ~ Double, PropagateColor a, a' ~ a) => Rotate (Quaternion d -> a -> a') where
  rotate q a = propagateColor (W.rotate (q ^. _yzw) (acos (q ^. _x))) a

instance {-# OVERLAPPABLE #-} (v ~ V3, ang ~ Double, PropagateColor a, a' ~ a) => Rotate (E v -> ang -> a -> a') where
  rotate (E e) ang a = propagateColor (W.rotate (0 & e .~ 1) (mod2pi ang)) a

class Rotated a where
  _rotated :: a

instance {-# INCOHERENT #-} (Profunctor p, Functor g, x ~ Double, y ~ Double, z ~ Double, ang ~ Double, PropagateColor a, a' ~ a) => Rotated (x -> y -> z -> ang -> Optic' p g a a') where
  _rotated x y z ang = iso (rotate x y z ang :: a' -> a) (rotate x y z (-ang) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, d ~ Double, ang ~ Double, PropagateColor a, a' ~ a) => Rotated (V3 d -> ang -> Optic' p g a a') where
  _rotated v ang = iso (rotate v ang :: a' -> a) (rotate v (-ang) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, d ~ Double, PropagateColor a, a' ~ a) => Rotated (Quaternion d -> Optic' p g a a') where
  _rotated q = iso (rotate q :: a' -> a) (propagateColor (W.rotate (negate (q ^. _yzw)) (acos (q ^. _x))) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, v ~ V3, ang ~ Double, PropagateColor a, a' ~ a) => Rotated (E v -> ang -> Optic' p g a a') where
  _rotated (E e) ang = iso (rotate (E e) ang :: a' -> a) (rotate (E e) (-ang) :: a -> a')

-- | Rotate by degrees around an axis specified in one of these ways:
class RotateDeg a where
  -- | @rotateDeg@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > rotateDeg x y z deg
  -- > rotateDeg v3 deg
  -- > rotateDeg q  deg -- ignore the quaternion's magnitude
  -- > rotateDeg ey deg
  rotateDeg :: a

fromDeg :: Double -> Double
fromDeg a = mod2pi (a * pi / 180)

instance {-# INCOHERENT #-} (deg ~ Double, x ~ Double, y ~ Double, z ~ Double, PropagateColor a, a' ~ a) => RotateDeg (x -> y -> z -> deg -> a -> a') where
  rotateDeg x y z d a = propagateColor (W.rotate (V3 x y z) (fromDeg d)) a

instance {-# OVERLAPPABLE #-} (deg ~ Double, d ~ Double, PropagateColor a, a' ~ a) => RotateDeg (V3 d -> deg -> a -> a') where
  rotateDeg v d a = propagateColor (W.rotate v (fromDeg d)) a

instance {-# OVERLAPPABLE #-} (deg ~ Double, d ~ Double, PropagateColor a, a' ~ a) => RotateDeg (Quaternion d -> deg -> a -> a') where
  rotateDeg q d = propagateColor (W.rotate (q ^. _yzw) (fromDeg d))

instance {-# OVERLAPPABLE #-} (deg ~ Double, v ~ V3, PropagateColor a, a' ~ a) => RotateDeg (E v -> deg -> a -> a') where
  rotateDeg (E e) d a = propagateColor (W.rotate (0 & e .~ 1) (fromDeg d)) a

-- \| @rotateDeg@ expressions of type 'Transformable' @a => Iso' a a@ (probably Iso 'Solid' 'Solid')
--
-- > _rotatedDeg x y z deg
-- > _rotatedDeg v3 deg
-- > _rotatedDeg q  deg -- ignore the quaternion's magnitude
-- > _rotatedDeg ey deg
class RotatedDeg a where
  _rotatedDeg :: a

instance {-# INCOHERENT #-} (Profunctor p, Functor g, deg ~ Double, x ~ Double, y ~ Double, z ~ Double, PropagateColor a, a' ~ a) => RotatedDeg (x -> y -> z -> deg -> Optic' p g a a') where
  _rotatedDeg x y z d = iso (rotateDeg x y z d :: a' -> a) (rotateDeg x y z (-d) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, deg ~ Double, d ~ Double, PropagateColor a, a' ~ a) => RotatedDeg (V3 d -> deg -> Optic' p g a a') where
  _rotatedDeg v d = iso (rotateDeg v d :: a' -> a) (rotateDeg v (-d) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, deg ~ Double, d ~ Double, PropagateColor a, a' ~ a) => RotatedDeg (Quaternion d -> deg -> Optic' p g a a') where
  _rotatedDeg q d = iso (rotateDeg q d :: a' -> a) (rotateDeg q (-d) :: a -> a')

instance {-# OVERLAPPABLE #-} (Profunctor p, Functor g, deg ~ Double, v ~ V3, PropagateColor a, a' ~ a) => RotatedDeg (E v -> deg -> Optic' p g a a') where
  _rotatedDeg (E e) d = iso (rotateDeg (E e) d :: a' -> a) (rotateDeg (E e) (-d) :: a -> a')

-- | Scale x y z axes
class Scale a where
  -- | @scale@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > scale v3
  -- > scale x y z
  -- > scale xy z
  -- > scale ex x
  -- > scale ey y
  --
  -- > v3 :: V3 Double
  -- > x,y,z,xy :: Double
  -- > ex, ey :: E V3
  scale, scaled :: a

instance {-# INCOHERENT #-} (Num a, v ~ V3, amt ~ Double, PropagateColor a, a' ~ a) => Scale (E v -> amt -> a -> a') where
  scale (E e) amt a = propagateColor (W.scale (1 & e .~ amt)) a
  scaled (E e) amt a = propagateColor (W.scale (1 & e .~ amt)) a + a

instance {-# INCOHERENT #-} (Num a, x ~ Double, y ~ Double, z ~ Double, PropagateColor a, a' ~ a) => Scale (x -> y -> z -> a -> a') where
  scale x y z a = propagateColor (W.scale (V3 x y z)) a
  scaled x y z a = propagateColor (W.scale (V3 x y z)) a + a

instance {-# INCOHERENT #-} (Num a, xy ~ Double, z ~ Double, PropagateColor a, a' ~ a) => Scale (xy -> z -> a -> a') where
  scale xy z a = propagateColor (W.scale (V3 xy xy z)) a
  scaled xy z a = propagateColor (W.scale (V3 xy xy z)) a + a

instance {-# OVERLAPS #-} (Num a, PropagateColor a, a' ~ a, Double ~ d) => Scale (d -> a -> a') where
  scale xyz a = propagateColor (W.uScale xyz) a
  scaled xyz a = propagateColor (W.uScale xyz) a + a

-- | Scale x y axes
class Scale2D a where
  -- | @scale2D@ expressions of type 'Transformable2D' @a => a -> a@ (probably 'Shape' -> 'Shape')
  --
  -- > scale2D v2
  -- > scale2D x y z
  -- > scale2D ex x
  -- > scale2D ey y
  scale2D, scaled2D :: a

instance {-# INCOHERENT #-} (Num a, v ~ V2, amt ~ Double, Transformable2D a, a' ~ a) => Scale2D (E v -> amt -> a -> a') where
  scale2D (E e) amt a = W.scale2D (1 & e .~ amt) a
  scaled2D (E e) amt a = W.scale2D (1 & e .~ amt) a + a

instance {-# OVERLAPPABLE #-} (Num a, x ~ Double, y ~ Double, Transformable2D a, a' ~ a) => Scale2D (x -> y -> a -> a') where
  scale2D x y a = W.scale2D (V2 x y) a
  scaled2D x y a = W.scale2D (V2 x y) a + a

instance {-# OVERLAPPABLE #-} (Num a, Transformable2D a, a' ~ a, Double ~ d) => Scale2D (d -> a -> a') where
  scale2D xy a = W.scale2D (V2 xy xy) a
  scaled2D xy a = W.scale2D (V2 xy xy) a + a

scaledOptic ::
  forall p g a a'.
  (Profunctor p, Functor g, PropagateColor a, a' ~ a) =>
  V3 Double ->
  Maybe (Optic' p g a a')
scaledOptic v
  | any WNZ.nearZero v = Nothing
  | otherwise = Just $ iso (propagateColor (W.scale v) :: a' -> a) (propagateColor (W.scale (1 / v)) :: a -> a')

class Scaled a where
  _scaled :: a

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, Profunctor p, Functor g, PropagateColor a, a' ~ a) => Scaled (d -> e -> f -> Maybe (Optic' p g a a')) where
  _scaled x y z = scaledOptic (V3 x y z)

instance {-# OVERLAPPABLE #-} (d ~ Double, Profunctor p, Functor g, PropagateColor a, a' ~ a) => Scaled (V3 d -> Maybe (Optic' p g a a')) where
  _scaled v = scaledOptic v

instance {-# OVERLAPPABLE #-} (xy ~ Double, z ~ Double, Profunctor p, Functor g, PropagateColor a, a' ~ a) => Scaled (xy -> z -> Maybe (Optic' p g a a')) where
  _scaled xy z = scaledOptic (V3 xy xy z)

instance {-# OVERLAPS #-} (d ~ Double, Profunctor p, Functor g, PropagateColor a, a' ~ a) => Scaled (d -> Maybe (Optic' p g a a')) where
  _scaled xyz = scaledOptic (V3 xyz xyz xyz)

instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, Profunctor p, Functor g, PropagateColor a, a' ~ a) => Scaled (E v -> amt -> Maybe (Optic' p g a a')) where
  _scaled (E e) amt = scaledOptic (1 & e .~ amt)

scaled2DOptic ::
  forall p g a a'.
  (Profunctor p, Functor g, Transformable2D a, a' ~ a) =>
  V2 Double ->
  Maybe (Optic' p g a a')
scaled2DOptic v
  | any WNZ.nearZero v = Nothing
  | otherwise = Just $ iso (W.scale2D v :: a' -> a) (W.scale2D (1 / v) :: a -> a')

class Scaled2D a where
  _scaled2D :: a

instance {-# INCOHERENT #-} (x ~ Double, y ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2D (x -> y -> Maybe (Optic' p g a a')) where
  _scaled2D x y = scaled2DOptic (V2 x y)

instance {-# OVERLAPPABLE #-} (d ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2D (V2 d -> Maybe (Optic' p g a a')) where
  _scaled2D v = scaled2DOptic v

instance {-# OVERLAPPABLE #-} (d ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2D (d -> Maybe (Optic' p g a a')) where
  _scaled2D xy = scaled2DOptic (V2 xy xy)

instance {-# OVERLAPPABLE #-} (v ~ V2, amt ~ Double, Profunctor p, Functor g, Transformable2D a, a' ~ a) => Scaled2D (E v -> amt -> Maybe (Optic' p g a a')) where
  _scaled2D (E e) amt = scaled2DOptic (1 & e .~ amt)

-- | Reflect across a plane through the origin the normal specified as a V3 Double, E V3
class Mirror a where
  -- | @mirror@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > mirror v3
  -- > mirror x y z
  -- > mirror ex x
  -- > mirror ey y
  -- > mirror ez z
  -- > mirror ex ey ez = mirror ex . mirror ey . mirror ez
  mirror :: a

  -- | @mirrored@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > mirrored v3 = \solid -> mirror v3 solid + solid
  -- > mirrored x y z
  -- > mirrored ex x
  -- > mirrored ey y
  -- > mirrored ez z
  -- > mirrored ex ey ez = mirrored ex . mirrored ey . mirrored ez
  mirrored :: a

instance {-# OVERLAPS #-} (Num s, vd ~ V3 Double, PropagateColor s, s' ~ s) => Mirror (vd -> s -> s') where
  mirror v a = propagateColor (W.mirror v) a
  mirrored v a = propagateColor (W.mirror v) a + a

instance {-# INCOHERENT #-} (Num s, v ~ V3, amt ~ Double, PropagateColor s, s' ~ s) => Mirror (E v -> s -> s') where
  mirror (E e) a = propagateColor (W.mirror (0 & e .~ 1)) a
  mirrored (E e) a = propagateColor (W.mirror (0 & e .~ 1)) a + a

instance {-# INCOHERENT #-} (Num s, v ~ V3, v ~ v', PropagateColor s, s' ~ s) => Mirror (E v -> E v' -> s -> s') where
  mirror (E f) (E g) a =
    let ga = propagateColor (W.mirror (0 & g .~ 1)) a
        fga = propagateColor (W.mirror (0 & f .~ 1)) ga
     in fga
  mirrored (E f) (E g) a =
    let ga = propagateColor (W.mirror (0 & g .~ 1)) a
        fga = propagateColor (W.mirror (0 & f .~ 1)) ga
     in fga + ga + a

instance {-# INCOHERENT #-} (Num s, v ~ V3, v ~ v', v ~ v'', PropagateColor s, s' ~ s) => Mirror (E v -> E v' -> E v'' -> s -> s') where
  mirror (E e) (E f) (E g) a =
    let ga = propagateColor (W.mirror (0 & g .~ 1)) a
        fga = propagateColor (W.mirror (0 & f .~ 1)) ga
        efga = propagateColor (W.mirror (0 & e .~ 1)) fga
     in efga
  mirrored (E e) (E f) (E g) a =
    let ga = propagateColor (W.mirror (0 & g .~ 1)) a
        fga = propagateColor (W.mirror (0 & f .~ 1)) ga
        efga = propagateColor (W.mirror (0 & e .~ 1)) fga
     in efga + fga + ga + a

instance {-# INCOHERENT #-} (Num s, x ~ Double, y ~ Double, z ~ Double, PropagateColor s, s ~ s') => Mirror (x -> y -> z -> s -> s') where
  mirror x y z a = propagateColor (W.mirror (V3 x y z)) a
  mirrored x y z a = propagateColor (W.mirror (V3 x y z)) a + a

class Mirrored a where
  _mirrored :: a

instance {-# OVERLAPS #-} (Num a, Profunctor p, Functor g, vd ~ V3 Double, PropagateColor a, a' ~ a) => Mirrored (vd -> Optic' p g a a') where
  _mirrored v = iso (mirror v :: a' -> a) (mirror v :: a -> a')

instance {-# INCOHERENT #-} (Num a, Profunctor p, Functor g, x ~ Double, y ~ Double, z ~ Double, PropagateColor a, a' ~ a) => Mirrored (x -> y -> z -> Optic' p g a a') where
  _mirrored x y z = iso (mirror x y z :: a' -> a) (mirror x y z :: a -> a')

instance {-# INCOHERENT #-} (Num a, Profunctor p, Functor g, v ~ V3, PropagateColor a, a' ~ a) => Mirrored (E v -> Optic' p g a a') where
  _mirrored (E e) = iso (mirror (E e) :: a' -> a) (mirror (E e) :: a -> a')

instance {-# INCOHERENT #-} (Num a, Profunctor p, Functor g, v ~ V3, v' ~ v, PropagateColor a, a' ~ a) => Mirrored (E v -> E v' -> Optic' p g a a') where
  _mirrored (E f) (E g) = iso (mirror (E f) (E g) :: a' -> a) (mirror (E f) (E g) :: a -> a')

instance {-# INCOHERENT #-} (Num a, Profunctor p, Functor g, v ~ V3, v' ~ v, v'' ~ v, PropagateColor a, a' ~ a) => Mirrored (E v -> E v' -> E v'' -> Optic' p g a a') where
  _mirrored (E e) (E f) (E g) = iso (mirror (E e) (E f) (E g) :: a' -> a) (mirror (E e) (E f) (E g) :: a -> a')

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

rectangle :: Double -> Double -> Path2D
rectangle w h = loophv [w, h, -w]

-- | `[h,v,h,v,h,v] -> Path2D`
-- with a final edge added to make a loop
--
-- rectangle above could be
-- > rectangle x y = makeShape (loophv [x, y, -x])
loophv :: [Double] -> Path2D
loophv hvdims =
  do
    zipWithM_ (\f d -> lineRelative2D (f d)) (cycle [\x -> V2 x 0, \y -> V2 0 y]) hvdims
    closeLoop2D
    `execState` (0, mempty)
    & snd

-- | pad is like freecad PartDesign::Pad. It sweeps a shape, or lofts a uScale2D version
class Pad a where
  pad :: a
  -- ^ pad expressions of type 'Shape' -> 'Solid'
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
sweep path shape = W.sweep (toPath path) (toShape shape)

instance Num Shape where
  (+) = W.union
  (-) = W.difference
  (*) = W.intersection
  fromInteger i = scale2D (fromInteger i) (fromInteger i) unitSquare
  abs = error "instance Num Shape missing abs"
  signum = error "instance Num Shape missing signum"

-- | `circle diameter` in the xy plane (z=0)
circle :: Double -> Path
circle ((/ 2) -> radius) =
  do
    arcVia3D d l
    arcVia3D u r
    `execPathState` r
  where
    u = V3 0 radius 0
    l = V3 (-radius) 0 0
    d = V3 0 (-radius) 0
    r = V3 radius 0 0

-- | `fustrum d1 d2 h` has a circle of d2 at z=h, and another circle of d1 at z=0
fustrum d1 d2 h = loft [circle d1, translate ez h (circle d2)]

-- * spirals

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
