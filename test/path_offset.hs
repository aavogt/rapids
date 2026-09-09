import Rapids

main = do
  let actual = volume ySolid
      expected = volume ySolid_
  print (actual, expected)
  if actual <= 0 || expected <= 0 || abs (actual - expected) > 0.1 * max actual expected
    then error "single-edge and two-edge offsets differ by more than 10%"
    else pure ()
  writeSTEP "volume_offset.step" ySolid


ySolid :: Solid
ySolid = fromPaths [ ln (V3 (w/2) (h/2) 0), ln (V3 (-w/2) (h/2) 0), ln (V3 0 (-h/2) 0) ]
  where
  ln to = line 0 to :: Path
  h = 2
  w = 1

ySolid_ :: Solid
ySolid_ = fromPaths [ ln (V3 (w/2) (h/2) 0), ln (V3 (-w/2) (h/2) 0), ln (V3 0 (-h/2) 0) ]
  where
  ln to = line 0 (V3 1e-3 0 0) <> line (V3 1e-3 0 0) to :: Path
  h = 2
  w = 1

fromPaths xs = foldMap (pad t . toShape . offset t 1) xs
  where t = 0.1
