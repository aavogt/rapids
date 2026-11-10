{-# LANGUAGE TemplateHaskell #-}
import Common
import Data.Map (restrictKeys)
import qualified Data.Set as Set
import Data.IORef

$(writeAction)

redCube = $red unitCube

main = do
  let p = pad 2 $ section redCube
  ks <- colorKeys p
  m <- readIORef colorAttrsMap
  print (volume p, m `restrictKeys` Set.fromList ks, m)
  -- the same for
  -- silhouette, revolution, offset, sweep, sweepRuled, loft2, fillet,chamfer,
  -- Rapids.Statistics.areasColor :: Solid -> [(Note, Double)]
  -- volumeColor is too hard
  writeModel p
