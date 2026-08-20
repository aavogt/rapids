module Rapids.ToPath where
import Waterfall
import Linear
import Data.List

class ToPath a where toPath :: a -> Path

instance (Double ~ d) => ToPath [V2 d] where toPath abspts = mconcat [line (V3 a b 0) (V3 c d 0) | V2 a b : V2 c d : _ <- tails abspts]

instance (Double ~ d) => ToPath [V3 d] where toPath abspts = mconcat [line a b | a : b : _ <- tails abspts]

instance {-# OVERLAPS #-} (path ~ Path) => ToPath path where toPath = id

