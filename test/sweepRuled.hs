{-# LANGUAGE QuasiQuotes #-}
import Rapids
import Rapids.SVG

main = do
  writeSTEP "sweepRuled.step" s
  print (volume s)

s = sweepRuled (\(V2 x y) -> foldMap (translate x y 0 . rotate ez (atan2 y x) ex (pi/2) . toPath)
      [svg|
        q 5,0,15,10
        h -5
        v 5
        h -5
        l -5,5
      |])
    (unitPolygon 6)
