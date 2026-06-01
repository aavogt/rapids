{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
    -- module Rapids.Color,
    module Rapids.IniVal,
    module Rapids.Path,
    module Linear,
    module Control.Lens,
    module Waterfall,
  )
where

import Control.Applicative
import Control.Lens hiding (prism)
-- import Rapids.Color

import Control.Monad
import Data.IORef
import Data.List (tails)
import Data.Maybe
import GHC.TypeLits
import Linear hiding (rotate)
import Rapids.IniVal
import Rapids.Path
import System.Directory
import System.FilePath
import Waterfall hiding
  ( appendPath,
    appendPath2D,
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
    closeLoop2D,
    closeLoop3D,
    difference,
    intersection,
    line2D,
    line3D,
    lineRelative2D,
    lineRelative3D,
    lineTo2D,
    lineTo3D,
    mirror,
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
    union,
    scale2D,
    translate2D,
  )
import qualified Waterfall as W
import Control.Monad
import GHC.TypeLits

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

instance {-# INCOHERENT #-} (d ~ Double, e ~ Double, f ~ Double, Transformable a, a' ~ a) => Translate (d -> e -> f -> a -> a') where
  translate x y z a = W.translate (V3 x y z) a

instance {-# OVERLAPPABLE #-} (d ~ Double, Transformable a, a ~ a') => Translate (V3 d -> a -> a') where
  translate v a = W.translate v a

-- | Linear defines 'ex' 'ey' 'ez'
--
-- > transform 'ex' 3 solid
instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, Transformable a, a' ~ a) => Translate (E v -> amt -> a -> a') where
  translate (E e) amt a = W.translate (0 & e .~ amt) a

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

-- | Rotate a 'Transformable' by radians around an axis specified in one of these ways:
class Rotate a where
  -- | @rotate@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > rotate x y z rad
  -- > rotate v     rad
  -- > rotate q     rad
  -- > rotate ex    rad
  rotate :: a

instance {-# INCOHERENT #-} (x ~ Double, y ~ Double, z ~ Double, ang ~ Double, Transformable a, a' ~ a) => Rotate (x -> y -> z -> ang -> a -> a') where
  rotate x y z ang a = W.rotate (V3 x y z) ang a

instance {-# OVERLAPPABLE #-} (d ~ Double, ang ~ Double, Transformable a, a' ~ a) => Rotate (V3 d -> ang -> a -> a') where
  rotate v ang a = W.rotate v ang a

instance {-# OVERLAPPABLE #-} (d ~ Double, Transformable a, a' ~ a) => Rotate (Quaternion d -> a -> a') where
  rotate q a = W.rotate (q ^. _yzw) (acos (q ^. _x)) a

instance {-# OVERLAPPABLE #-} (v ~ V3, ang ~ Double, Transformable a, a' ~ a) => Rotate (E v -> ang -> a -> a') where
  rotate (E e) ang a = W.rotate (0 & e .~ 1) ang a

-- | Rotate by degrees around an axis specified in one of these ways:
class RotateBy a where
  -- | @rotateDeg@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > rotateDeg x y z deg
  -- > rotateDeg v3 deg
  -- > rotateDeg q  deg
  -- > rotateDeg ey deg
  rotateDeg :: a

instance {-# INCOHERENT #-} (deg ~ Double, x ~ Double, y ~ Double, z ~ Double, Transformable a, a' ~ a) => RotateBy (x -> y -> z -> deg -> a -> a') where
  rotateDeg x y z p a = W.rotate (V3 x y z) (p * pi / 180) a

instance {-# OVERLAPPABLE #-} (deg ~ Double, d ~ Double, Transformable a, a' ~ a) => RotateBy (V3 d -> deg -> a -> a') where
  rotateDeg v p a = W.rotate v (p * pi / 180) a

instance {-# OVERLAPPABLE #-} (deg ~ Double, d ~ Double, Transformable a, a' ~ a) => RotateBy (Quaternion d -> deg -> a -> a') where
  rotateDeg q p = W.rotate (q ^. _yzw) (p * pi / 180)

instance {-# OVERLAPPABLE #-} (deg ~ Double, v ~ V3, Transformable a, a' ~ a) => RotateBy (E v -> deg -> a -> a') where
  rotateDeg (E e) p a = W.rotate (0 & e .~ 1) (p * pi / 180) a

-- | Scale x y z axes
class Scale a where
  -- | @scale@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > scale v3
  -- > scale x y z
  -- > scale ex x
  -- > scale ey y
  scale :: a

instance {-# INCOHERENT #-} (v ~ V3, amt ~ Double, Transformable a, a' ~ a) => Scale (E v -> amt -> a -> a') where
  scale (E e) amt a = W.scale (1 & e .~ amt) a

instance {-# OVERLAPPABLE #-} (x ~ Double, y ~ Double, z ~ Double, Transformable a, a' ~ a) => Scale (x -> y -> z -> a -> a') where
  scale x y z a = W.scale (V3 x y z) a

instance {-# OVERLAPPABLE #-} (Transformable a, a' ~ a, Double ~ d) => Scale (d -> a -> a') where
  scale xyz a = W.scale (V3 xyz xyz xyz) a

-- | Scale x y axes
class Scale2D a where
  -- | @scale2D@ expressions of type 'Transformable2D' @a => a -> a@ (probably 'Shape' -> 'Shape')
  --
  -- > scale2D v2
  -- > scale2D x y z
  -- > scale2D ex x
  -- > scale2D ey y
  scale2D :: a

instance {-# INCOHERENT #-} (v ~ V2, amt ~ Double, Transformable2D a, a' ~ a) => Scale2D (E v -> amt -> a -> a') where
  scale2D (E e) amt a = W.scale2D (1 & e .~ amt) a

instance {-# OVERLAPPABLE #-} (x ~ Double, y ~ Double, Transformable2D a, a' ~ a) => Scale2D (x -> y -> a -> a') where
  scale2D x y a = W.scale2D (V2 x y) a

instance {-# OVERLAPPABLE #-} (Transformable2D a, a' ~ a, Double ~ d) => Scale2D (d -> a -> a') where
  scale2D xy a = W.scale2D (V2 xy xy) a

-- | Reflect across a plane through the origin
class Mirror a where
  -- | @mirror@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > mirror v3
  -- > mirror x y z
  -- > mirror ex x
  -- > mirror ey y
  -- > mirror ez z
  mirror :: a

instance {-# INCOHERENT #-} (d ~ Double, Transformable s, s' ~ s) => Mirror (V3 d -> s -> s') where
  mirror v a = W.mirror v a

instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, Transformable s, s' ~ s) => Mirror (E v -> s -> s') where
  mirror (E e) a = W.mirror (0 & e .~ 1) a

instance {-# OVERLAPPABLE #-} (x ~ Double, y ~ Double, z ~ Double, Transformable s, s ~ s') => Mirror (x -> y -> z -> s -> s') where
  mirror x y z a = W.mirror (V3 x y z) a

-- | mirror plus the original both the original and the image as in freecad's PartDesign::Mirrored
class Mirrored a where
  -- | @mirrored@ expressions of type 'Transformable' @a => a -> a@ (probably 'Solid' -> 'Solid')
  --
  -- > mirrored v3
  -- > mirrored x y z
  -- > mirrored ex x
  mirrored :: a

instance {-# INCOHERENT #-} (d ~ Double, Num s, Transformable s, s' ~ s) => Mirrored (V3 d -> s -> s') where
  mirrored v a = W.mirror v a + a

instance {-# OVERLAPPABLE #-} (v ~ V3, amt ~ Double, Num s, Transformable s, s' ~ s) => Mirrored (E v -> s -> s') where
  mirrored (E e) a = W.mirror (0 & e .~ 1) a + a

instance {-# OVERLAPPABLE #-} (x ~ Double, y ~ Double, z ~ Double, Num s, Transformable s, s ~ s') => Mirrored (x -> y -> z -> s -> s') where
  mirrored x y z a = W.mirror (V3 x y z) a + a

-- | needed for `instance Mirrored (_ -> Path -> Path)`
instance Num Path where
  (+) = (<>)

-- (-) could remove points, but maybe it needs to defer evaluation ie. store a sign bool (Bool, Path)
-- because x + (0 - x) is supposed to work?

instance Num Solid where
  (-) = W.difference
  (+) = W.union
  (*) = W.intersection
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

rectangle :: Double -> Double -> Shape
rectangle x y = scale2D x y unitSquare

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
class (PadN (NArgs a) a) => Pad a where
  pad :: a
  -- ^ pad expressions of type 'Shape' -> 'Solid'
  --
  -- > pad z
  -- > pad z taperFrac
  -- > pad x y z
  -- > pad x y z taperFrac
  -- > pad v
  -- > pad v taperFrac

instance (PadN (NArgs a) a) => Pad a where pad = padN

class (NArgs a ~ n) => PadN n a where
  padN :: a

type family NArgs f where
  NArgs (a -> b) = 1 + NArgs b
  NArgs f = 0

-- should this sweep (line 0 (V3 0 0 eps) <> ?)
instance {-# INCOHERENT #-} (Double ~ double, ToShape shape, Solid ~ solid) => PadN 2 (double -> shape -> solid) where
  padN z = W.sweep (line 0 (V3 0 0 z)) . toShape

instance (Double ~ double, Double ~ taper, ToShape shape, Solid ~ solid) => PadN 3 (double -> double -> shape -> solid) where
  padN z taperFrac = padN (V3 z 0 0) taperFrac . toShape

instance {-# OVERLAPPABLE #-} (Double ~ double, ToShape shape, Solid ~ solid) => PadN 2 (V3 double -> shape -> solid) where
  padN xyz = W.sweep (line 0 xyz) . toShape

instance (PadN 5 (d -> d -> d -> taper -> shape -> solid), Double ~ d, Double ~ taper, ToShape shape, Solid ~ solid) => PadN 3 (V3 d -> taper -> shape -> solid) where
  padN (V3 x y z) taperFrac shape = padN x y z taperFrac shape

instance (Double ~ x, Double ~ y, Double ~ z, Double ~ taper, ToShape shape, Solid ~ solid) => PadN 4 (x -> y -> z -> shape -> solid) where
  padN x y z = W.sweep (line 0 (V3 x y z)) . toShape

instance (Double ~ x, Double ~ z, Double ~ y, Double ~ taper, ToShape shape, Solid ~ solid) => PadN 5 (x -> y -> z -> taper -> shape -> solid) where
  padN x y z taperFrac shape =
    unions
      [ loft [fromPath2D q, p]
        | q <- shapePaths (toShape shape),
          let p = translate x y z (fromPath2D (uScale2D taperFrac q))
      ]

class ToShape a where toShape :: a -> Shape

class ToPath a where toPath :: a -> Path

instance ToShape Shape where toShape = id

instance ToShape Path2D where toShape = makeShape

instance (Double ~ d) => ToShape [V2 d] where toShape abspts = makeShape $ mconcat [line a b :: Path2D | a : b : _ <- tails abspts]

instance (Double ~ d) => ToPath [V2 d] where toPath abspts = mconcat [line (V3 a b 0) (V3 c d 0) | V2 a b : V2 c d : _ <- tails abspts]

instance (Double ~ d) => ToPath [V3 d] where toPath abspts = mconcat [line a b | a : b : _ <- tails abspts]

sweep path shape = W.sweep (toPath path) (toShape shape)

instance Num Shape where
  (+) = W.union
  (-) = W.difference
  (*) = W.intersection
  fromInteger i = rectangle (fromInteger i) (fromInteger i)
