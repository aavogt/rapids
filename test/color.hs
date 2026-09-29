{-# LANGUAGE TemplateHaskell #-}
import Rapids
import Rapids.Color
import Data.Map (restrictKeys)
import qualified Data.Set as Set
import Data.IORef

redCube = $red unitCube

main = do
  fks <- colorKeys $ pad 2 $ section redCube
  m <- readIORef colorAttrsMap
  print (m `restrictKeys` Set.fromList fks, m)
