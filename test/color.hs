{-# LANGUAGE TemplateHaskell #-}
import Rapids
import Rapids.Color
import Data.Map (restrictKeys)
import qualified Data.Set as Set
import Data.IORef

redCube = $red unitCube

main = do
  fks <- faceKeys $ pad 2 $ section redCube
  m <- readIORef faceAttrsMap
  print (m `restrictKeys` Set.fromList fks, m)
