{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
import Rapids

main = do
  return ()
  print (volume $ pad 2 roundedRectangle1, volume $ pad 2 $ roundedRectangle)

roundedRectangle = execPathState0 do
  r 100
  u 100
  l 100
  d 100
  fillets radius
 where
  u y = lineRelative3D (V3 0 y 0)
  d y = u (-y)
  r x = lineRelative3D (V3 x 0 0)
  l x = r (-x)

radius = 10

roundedRectangle1 = do
  r (100 - 2 * radius)
  arcRelative Clockwise radius (V2 radius radius)
  u (100 - 2 * radius)
  arcRelative Clockwise radius (V2 (-radius) radius)
  l (100 - 2 * radius)
  arcRelative Clockwise radius (V2 (-radius) (-radius))
  d (100 - 2 * radius)
  arcRelative Clockwise radius (V2 radius (-radius))
 `execPathState` V2 radius 0
 where
  u y = lineRelative2D (V2 0 y)
  d y = u (-y)
  r x = lineRelative2D (V2 x 0)
  l x = r (-x)
