import Rapids

ring = pad 1 $ section $ scale 1.1 unitSphere - unitSphere

area r = pi * r^2

main = do
  print (volume ring, area 1.1 - area 1)
  writeSTEP "ring.step" ring
