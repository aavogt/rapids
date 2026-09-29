module Rapids.Path where

import Rapids.Reexports

-- | rectangle centered at the origin. These have equivalent geometry:
--
-- > rectangle w h
-- > section (box (V3 w h 1))
-- > section (scale w h 1 unitCube)
--
-- also 'unitSquare'
rectangle :: Double -> Double -> Path2D
rectangle w h = pathFrom 0 [lineTo (V2 w 0), lineTo (V2 w h), lineTo (V2 0 h), lineTo 0]

-- | @circle diameter@ in the xy plane (z=0)
--
-- also 'unitCircle'
circle :: Double -> Path
circle ((/ 2) -> radius) = pathFrom a [arcViaTo d c, arcViaTo b a]
  where
    d = V3 0 (-radius) 0
    c = V3 (-radius) 0 0
    a = V3 radius 0 0
    b = V3 0 radius 0
