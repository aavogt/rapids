import Rapids
ring = pad 1 $ section $ scale 1.1 unitSphere - unitSphere + scale 0.9 unitSphere
boxbox = pad 2 $ section $ unitCube + 2
boxbox2 = pad 2 $ section $ translate ey (-0.5) unitCube + 2

area r = pi * r^2

main = do
  let actualBox = volume boxbox
      actualBox2 = volume boxbox2
  writeSTEP "boxbox.step" boxbox
  print actualBox
  writeSTEP "boxbox2.step" boxbox2
  print (volume 2, actualBox2)
  if abs (actualBox - 8) > 1e-6 || abs (actualBox2 - 9) > 1e-6
    then error "section volumes are incorrect"
    else pure ()
  print (volume ring, area 1.1 - area 1 + area 0.9)
  writeSTEP "ring.step" ring
