import Rapids
import Solids

main = do
  print [(x, volume (offset x bowtie)) | x <- [0.01, 0.1, 0.5, 1, 2]]
  writeSTEP "volume_offset.step" ySolid


ySolid :: Solid
ySolid = fromPaths [ ln (V3 (w/2) (h/2) 0), ln (V3 (-w/2) (h/2) 0), ln (V3 0 (-h/2) 0) ]
  where
  ln to = line 0 to :: Path
  h = 2
  w = 1

fromPaths xs = foldMap (pad t . toShape . offset t 1) xs
  where t = 0.1
