import Rapids

drill :: Double -> Solid
drill z = scale 2 centeredCube - translate ez (-1) (scale 1 z unitCylinder)

hole = pad 1 $ silhouette $ drill 2

closed = pad 1 $ silhouetteShape $ silhouetteSolid $ drill 2

closed2 = pad 1 $ silhouette $ drill 1

main = do
  print (volume hole, 4 - pi)
  print (volume closed2, volume closed, 4)
  writeSTEP "silhouette.step" hole
