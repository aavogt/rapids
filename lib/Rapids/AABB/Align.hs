-- | arrange a pair of solids relative to each other using their 'axisAlignedBoundingBox',
-- like Inkscape's <https://inkscape-manuals.readthedocs.io/en/latest/align-and-distribute.html Align and Distribute>.
--
-- The direction is "Linear"'s @ex ey ez@.
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
import Rapids.AABB.Lens

-- * interface
-- ** alignment where the first solid isn't added to the result

-- | @stack ex a b@ moves @b@ along the x axis so the left side of @b@ is coplanar with @a@'s right side.
-- Only the moved @b@ is returned.
--
-- > stack ez a b == translate ez z b
--
-- where @z@ makes the bottom of @b@ coplanar with the top of @a@.
stack :: E V3 -> Solid -> Solid -> Solid
stack (E el) a b = fromJust do
  (a0, a1) <- axisAlignedBoundingBox a
  (b0, b1) <- axisAlignedBoundingBox b
  let aVal = a1 ^. el
  let bVal = b0 ^. el
  Just $ translate (E el) (aVal - bVal) b

-- | @center ez a b@ moves @b@ so that the lines connecting opposite 'axisAlignedBoundingBox' faces
-- are collinear. Only the moved @b@ is returned.
--
-- > center ez a b == translate (V3 x y 0) b
--
-- where @x@ @y@ make the centers of the z-faces collinear.
center :: E V3 -> Solid -> Solid -> Solid
center (E el) a b = fromJust do
  (a0, a1) <- axisAlignedBoundingBox a
  (b0, b1) <- axisAlignedBoundingBox b
  let abMid = (a0+a1)/2 - (b0+b1)/2
  Just $ translate (abMid & el .~ 0) b

-- | @left ex a b = translate ex t b@ with @t@ such that the left aabb sides align.
left :: E V3 -> Solid -> Solid -> Solid
left (E el) a b = fromJust do
  aVal <- a ^? aabb . _Just . _1 . el
  bVal <- b ^? aabb . _Just . _1 . el
  Just $ translate (E el) (aVal - bVal) b
  -- one translate _ 0 should always be id, same for right

-- | @right ex a b = translate ex t b@ with @t@ such that the right aabb sides align.
right :: E V3 -> Solid -> Solid -> Solid
right (E el) a b = fromJust do
  aVal <- a ^? aabb . _Just . _2 . el
  bVal <- b ^? aabb . _Just . _2 . el
  Just $ translate (E el) (aVal - bVal) b

-- ** alignment where the first solid is added to the result

-- | @stacked ez a b = a + stack ez a b@
--
-- the analogue of @mirrored@ for 'stack'
stacked :: E V3 -> Solid -> Solid -> Solid
stacked el a b = a + stack el a b

-- | @centered ez a b = a + center ez a b@
centered :: E V3 -> Solid -> Solid -> Solid
centered el a b = a + center el a b

-- | > a `above` b
--
-- places the bottom of @a@ at the top of @b@ and adds @b@.
-- The convention is backwards compared to 'stacked', which would be @flip (stacked ez)@
-- and 'above' may be flipped and called below.
above :: Solid -> Solid -> Solid
a `above` b = fromJust do
   (_, V3 _ _ b2) <- axisAlignedBoundingBox b
   (V3 _ _ a1, _) <- axisAlignedBoundingBox a
   let dz = b2 - a1
   Just $ translate ez dz a + b

-- | @lefted ex a b = a + left ex a b@
lefted :: E V3 -> Solid -> Solid -> Solid
lefted el a b = a + left el a b

-- | @righted ex a b = a + right ex a b@
righted :: E V3 -> Solid -> Solid -> Solid
righted el a b = a + right el a b

-- Data.Semigroup.Min can't do this because it needs `instance Bounded Double`,
data MinMaxSumCount = MinMaxSumCount !Double !Double !Double Int

instance Semigroup MinMaxSumCount where
  MinMaxSumCount a b c n <> MinMaxSumCount d e f m = MinMaxSumCount (min a d) (max b e) (c + f) (n+m)

instance Monoid MinMaxSumCount where
  mempty = MinMaxSumCount (1/0) (-(1/0)) 0 0

-- | @distributed ez solids = unions (distribute ez solids)@
distributed :: E V3 -> [Solid] -> Solid
distributed e solids = unions (distribute e solids)

-- | @[sa,sb,sc] = distribute ez [s1,s2,s3]@: @sa@ is the lowest, @sb@ is the middle, @sc@ is the highest.
-- The middle elements are translated along z for equal gaps (or overlaps).
-- Note the result is sorted along the axis, not in the order of the input list.
distribute :: E V3 -> [Solid] -> [Solid]
distribute _ [] = []
distribute _ [s] = [s]
distribute (E el) solids = fromJust do
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
