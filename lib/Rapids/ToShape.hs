-- | overloaded conversion to a 'Shape' so that functions like 'Rapids.pad' and 'Rapids.revolution' can take paths
module Rapids.ToShape where
import Waterfall
import Rapids.Path.Project (projectPath)
import Linear
import Data.List

-- | @toShape@ is defined for @[V2 Double]@ (a polyline), 'Path2D', 'Path' (z is dropped), 'Shape'
-- and lists of any of those (combined with 'foldMap').
class ToShape a where toShape :: a -> Shape

instance ToShape a => ToShape [a] where toShape = foldMap toShape

instance (Double ~ d) => ToShape [V2 d] where toShape abspts = makeShape $ mconcat [line a b :: Path2D | a : b : _ <- tails abspts]

instance ToShape Shape where toShape = id

instance ToShape Path2D where toShape = makeShape

-- | uses 'projectPath' discarding z
instance ToShape Path where toShape = makeShape . projectPath

