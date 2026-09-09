module Rapids.Color.AxisTriad where

import Rapids.Reexports
import Rapids.Scale
import Rapids.Translate
import Rapids.Rotate
import Rapids.Num
import Rapids.Path.Offset
import Rapids.ToShape
import Rapids.Pad
import Rapids.Path
import Rapids.Color
import Rapids.ToPath

axisTriad :: Solid
axisTriad =
    $blue (rotate ex (pi / 2) (line ySolid)) +
      $green (rotate ey (pi / 2) (line zSolid)) +
       $red (line xSolid)
  where
    line label =
      translate ez 2 (scale 0.1 4 centeredCylinder)
        + translate ez 4 label

oSolid :: Solid
oSolid = fromPaths [ scale w h 1 (circle 1) ]

ySolid :: Solid
ySolid = fromPaths [ ln (V3 (w/2) (h/2) 0), ln (V3 (-w/2) (h/2) 0), ln (V3 0 (-h/2) 0) ]
  where ln to = line 0 (V3 1e-3 0 0) <> line (V3 1e-3 0 0) to

zSolid :: Solid
zSolid = fromPaths [ toPath $ rotate2D angle $ execPathState0 do
  lineTo2D (V2 (w/2) (h/2)) 
  lineTo2D (V2 (-w/2) (h/2))
  | angle <- [0, pi]
 ]

xSolid = fromPaths [ line 0 (V3 a b 0) <> line (V3 a b 0) (V3 (a+1e-3) b 0) | a <- [-w/2, w/2], b <- [-h/2,h/2] ]

fromPaths = foldMap (pad t . toShape . offsetPath 1 t)

w = 1
h = 2
t = 0.1
