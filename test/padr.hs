
import Control.Monad (unless)
import Rapids

main = do
  let actualVolume = volume solid
  print actualVolume
  unless (abs (actualVolume - 5.944752261871913) < 1e-3) $
    error "sweepRuled padr must include both end faces"
  writeSTEP "padr.step" solid

solid = padr 1 $ translate2D 1 0 $ rectangle 2 3
padr height sketch =
  sweepRuled
    ( \(V2 x y) ->
        let r = x
            v th = V3 (r * cos th) (r * sin th) y
            th = height / r / 2
            a = v (-th)
            b = v 0
            c = v th
         in arcVia a b c :: Path
    )
    sketch
