-- |  Waterfall suggests
--
-- 'pathFrom' :: Monoid path => point -> [point -> (point, path)] -> path
--
-- here is another take on that with the transformers package "Control.Monad.Trans.State".'State' type
-- so that:
--
-- > pathFrom p0 [line3D v, line3D w]
--
-- > do
-- >     line3D v
-- >     line3D w
-- >  `execState` p0 & snd
--
-- or execPathState0 to start with mempty/zero/origin
module Rapids.Path
  ( module Rapids.Path,
    module Rapids.Path.CornerOp,
    module Rapids.Path.Offset
  )
where

import Rapids.Path.Offset
import Linear
import Rapids.Path.CornerOp
import Waterfall (Path, Path2D)
import Waterfall.Path
import qualified Waterfall.TwoD.Path2D as W

rectangle :: Double -> Double -> Path2D
rectangle w h = pathFrom 0 [lineTo (V2 w 0), lineTo (V2 w h), lineTo (V2 0 h), lineTo 0]

-- | `circle diameter` in the xy plane (z=0)
circle :: Double -> Path
circle ((/ 2) -> radius) = pathFrom a [arcViaTo d c, arcViaTo b a]
  where
    d = V3 0 (-radius) 0
    c = V3 (-radius) 0 0
    a = V3 radius 0 0
    b = V3 0 radius 0
