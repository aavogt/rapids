-- | overloaded conversion to a 'Path' so that functions like 'Rapids.sweep' can take lists of points
module Rapids.ToPath where
import Waterfall
import Linear
import Data.List

-- | @toPath@ is defined for @[V2 Double]@ and @[V3 Double]@ (polylines through the points), 'Path2D' and 'Path'.
class ToPath a where toPath :: a -> Path

instance (Double ~ d) => ToPath [V2 d] where toPath abspts = mconcat [line (V3 a b 0) (V3 c d 0) | V2 a b : V2 c d : _ <- tails abspts]

instance (Double ~ d) => ToPath [V3 d] where toPath abspts = mconcat [line a b | a : b : _ <- tails abspts]

instance {-# OVERLAPS #-} (path ~ Path) => ToPath path where toPath = id

instance ToPath Path2D where toPath = fromPath2D

