module Rapids.ToShape where
import Waterfall
import Rapids.Path.Project (projectPath)
import Linear
import Data.List

class ToShape a where toShape :: a -> Shape

instance ToShape a => ToShape [a] where toShape = foldMap toShape

instance (Double ~ d) => ToShape [V2 d] where toShape abspts = makeShape $ mconcat [line a b :: Path2D | a : b : _ <- tails abspts]

instance ToShape Shape where toShape = id

instance ToShape Path2D where toShape = makeShape

-- | uses 'projectPath' discarding z
instance ToShape Path where toShape = makeShape . projectPath

