module Rapids.AABB.Align where

import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Array (allocaArray, peekArray)
import InlineOCCT
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Data.List (mapAccumL, sortOn)
import Linear (V3 (..), ez)
import Waterfall (Solid)
import Rapids.Color
import Waterfall.Internal.Finalizers (unsafeFromAcquire)
import Linear.Vector
import Control.Lens
import Data.Maybe
import Rapids.Translate
import Rapids.Num
import Rapids.AABB

-- | > a `above` b
--
-- places the bottom of `a` at the top of `b`
--
-- doesn't fit with the rest the convention is backwards so will be flipped and called below?
-- I'm not sure I covered all the options, because opposite side and same side have two orderings,
-- you could swap `a` and `b` in 3 places. but it seems that stack center left right are enough.
a `above` b = fromJust do
   (_, V3 _ _ b2) <- axisAlignedBoundingBox b
   (V3 _ _ a1, _) <- axisAlignedBoundingBox a
   let dz = b2 - a1
   Just $ translate ez dz a + b


-- | @stack ex a b@ moves `b` along the x axis so the left side is coplanar with `a`'s right side.
--
-- > above = flip (stack ez)
stacked el a b = a + stack el a b

-- | @center ex a b@ moves b so that the lines connecting opposite axisAlignedBoundingBox faces
-- are colllinear.
centered el a b = a + center el a b

-- | > left ex a b
--
-- aligns the left side of `a` and `b` (along the x axis)
-- by moving the one that's farther right leftwards
lefted el a b = a + left el a b

-- | > right ez a b
--
-- aligns the right (top) side of `a` and `b` (along the z axis)
-- by moving the one that's farther down upwards
righted el a b = a + right el a b

-- ** alignment where the first argument isn't added to the result

stack (E el) a b = fromJust do
  (a0, a1) <- axisAlignedBoundingBox a
  (b0, b1) <- axisAlignedBoundingBox b
  let aVal = a1 ^. el
  let bVal = b0 ^. el
  Just $ translate (E el) (aVal - bVal) b

center (E el) a b = fromJust do
  (a0, a1) <- axisAlignedBoundingBox a
  (b0, b1) <- axisAlignedBoundingBox b
  let abMid = (a0+a1)/2 - (b0+b1)/2
  Just $ translate (abMid & el .~ 0) b

left :: E V3 -> Solid -> Solid -> Solid
left (E el) a b = fromJust do
  (a0, a1) <- axisAlignedBoundingBox a
  (b0, b1) <- axisAlignedBoundingBox b
  let aVal = a0 ^. el
  let bVal = b0 ^. el
  let val = min aVal bVal
  Just $ translate (E el) (val - aVal) a + translate (E el) (val - bVal) b
  -- one translate _ 0 should always be id, same for right

right :: E V3 -> Solid -> Solid -> Solid
right (E el) a b = fromJust do
  (a0, a1) <- axisAlignedBoundingBox a
  (b0, b1) <- axisAlignedBoundingBox b
  let aVal = a1 ^. el
  let bVal = b1 ^. el
  let val = max aVal bVal
  Just $ translate (E el) (val - aVal) a + translate (E el) (val - bVal) b

-- Data.Semigroup.Min can't do this because it needs `instance Bounded Double`,
data MinMaxSumCount = MinMaxSumCount !Double !Double !Double Int

instance Semigroup MinMaxSumCount where
  MinMaxSumCount a b c n <> MinMaxSumCount d e f m = MinMaxSumCount (min a d) (max b e) (c + f) (n+m)

instance Monoid MinMaxSumCount where
  mempty = MinMaxSumCount (1/0) (-(1/0)) 0 0

distribute :: E V3 -> [Solid] -> Solid
distribute e solids = unions (distributed e solids)

distributed :: E V3 -> [Solid] -> [Solid]
distributed _ [] = []
distributed _ [s] = [s]
distributed (E el) solids = fromJust do
  aabbs <- mapM axisAlignedBoundingBox solids
  let f (view el -> l, view el -> r) = MinMaxSumCount l r (r-l) 1
  MinMaxSumCount l r occupied count <- Just (foldMap f aabbs)
  let h = ((r - l) - occupied) / (fromIntegral count - 1)
  let g s ((l,r), solid) = (s + h + r - l, (s - l, solid))
  Just $ [ translate (E el) lp solid
    | (lp, solid)
       <- zip aabbs solids
        & traversed . _1 . both %~ view el
        & sortOn (^. _1 . _1)
        & mapAccumL g 0
        & snd ]
